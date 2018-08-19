use instruments::{RcInstrument, Instrument, DEDUP_INSTRUMENT};
use instruments::assets::{RcCurrency, Currency, DEDUP_CURRENCY};
use pricers::RcPricerFactory;
use data::fixings::RcFixingTable;
use risk::RcReportGenerator;
use risk::marketdata::RcMarketData;
use core::dedup::{Dedup, DedupControl, dedup_map_from_slice};
use core::factories::Qrc;
use core::qm;
use serde::Deserialize;
use serde::Serialize;
use serde_json as sdj;
use std::io::{Read, Write};
use std::rc::Rc;

/// An instrument in QuantMath represents any tradable item. At simplest, it can be
/// a currency, zero coupon bond, equity or commodity. At most complex, it can be
/// a dynamic index or complex exotic product.
/// 
/// Most instruments have recursive definitions, depending on other instruments and
/// currencies. (A currency is a type of instrument, but is normally treated separately
/// as currencies cannot normally be replaced with other instruments.) The subcomponents
/// can be passed into this call, or expanded inline. Control of this behaviour is by
/// the ccy_dedup and instr_dedup parameters, and precomputed currencies and instruments
/// may be passed into this call (depending on the dedup parameters).
/// 
/// The source is any source of UTF-8 bytes, such as a file or string.
pub fn instrument_from_json(source: &mut Read,
    ccy_dedup: DedupControl, currencies: &[RcCurrency],
    instr_dedup: DedupControl, instruments: &[RcInstrument]) 
    -> Result<RcInstrument, qm::Error> {

    // create a deserializer based on the input
    let mut deserializer = sdj::Deserializer::from_reader(source);

    // convert the currencies and instrument components to maps, for efficient deduplication
    let currencies_map = dedup_map_from_slice(currencies);
    let instruments_map = dedup_map_from_slice(instruments);

    let mut ccy = Dedup::<Currency, Rc<Currency>>::new(ccy_dedup, currencies_map);
    let mut opt = Dedup::<Instrument, Qrc<Instrument>>::new(instr_dedup, instruments_map);
    let instr = ccy.with(&DEDUP_CURRENCY, 
        || opt.with(&DEDUP_INSTRUMENT,
        || RcInstrument::deserialize(&mut deserializer)))?;
    Ok(instr)
}

/// A currency in QuantMath normally represents a literal currency such as USD. For simplicity
/// we recommend that only major currencies are used. Thus a penny is represented as 0.01 GBP
/// rather than 1 gbp. Currencies may also be used to represent some precious metal commodities
/// such a XAU.
/// 
/// A currency is also an instrument, though normally currencies and instruments are treated
/// separately. An exception would be where a currency is contained in a basket, meaning cash
/// that is not discounted.
/// The source is any source of UTF-8 bytes, such as a file or string.
pub fn currency_from_json(source: &mut Read) 
    -> Result<RcCurrency, qm::Error> {

    // create a deserializer based on the input and use it to deserialize the currency
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let currency = RcCurrency::deserialize(&mut deserializer)?;
    Ok(currency)
}

/// A PricerFactory is used to create pricers, given an instrument to price and a fixing table.
/// It contains enough information to define how the pricing is done, for example whether it is
/// Monte-Carlo, how many paths, and which model to use.
pub fn pricer_factory_from_json(source: &mut Read)
    -> Result<RcPricerFactory, qm::Error> {

    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let pricer_factory = RcPricerFactory::deserialize(&mut deserializer)?;
    Ok(pricer_factory)
}

/// A FixingTable contains historical fixings for one or more assets, defined by Date and time of
/// day.
pub fn fixing_table_from_json(source: &mut Read)
    -> Result<RcFixingTable, qm::Error> {

    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let fixing_table = RcFixingTable::deserialize(&mut deserializer)?;
    Ok(fixing_table)
}

/// MarketData contains all the live market data required for pricing. This is spots, yield
/// and borrow curves, dividends, volatilities, and correlations. You can instantiate a
/// MarketData as here by deserializing it from json. It may be more efficient to instantiate
/// it directly, by using methods in the data module.
pub fn market_data_from_json(source: &mut Read)
    -> Result<RcMarketData, qm::Error> {

    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let market_data = RcMarketData::deserialize(&mut deserializer)?;
    Ok(market_data)
}

/// A ReportGenerator takes a pricer and bumps and revalues it to generate a report
/// of sensitivities to market data changes.
pub fn report_generator_from_json(source: &mut Read)
    -> Result<RcReportGenerator, qm::Error> {

    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let report_generator = RcReportGenerator::deserialize(&mut deserializer)?;
    Ok(report_generator)
}

/// Performs a calculation, outputting the price, and writing any reports that are
/// requested.
pub fn calculate(pricer_factory: RcPricerFactory, instrument: RcInstrument, 
    fixing_table: RcFixingTable, market_data: RcMarketData,
    report_generators: &[RcReportGenerator], pretty: bool, out: &mut Write)
    -> Result<f64, qm::Error> {

    let mut pricer = pricer_factory.new(instrument, fixing_table, market_data)?;
    let price = pricer.price()?;
    let mut saveable = pricer.as_bumpable().new_saveable();

    let mut reports = Vec::with_capacity(report_generators.len());
    for report_generator in report_generators.iter() {
        let report = report_generator.generate(&mut *pricer, &mut *saveable, price)?;
        reports.push(report);
    }

    serialize_output(&reports, pretty, out)?;

    Ok(price)
}

fn serialize_output<T>(to_write: &T, pretty: bool, out: &mut Write) -> Result<(), qm::Error>
where T: Serialize {
    if pretty {
        let mut serializer = sdj::Serializer::pretty(out);
        to_write.serialize(&mut serializer)?;
    } else {
        let mut serializer = sdj::Serializer::new(out);
        to_write.serialize(&mut serializer)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use math::numerics::{approx_eq, ApproxEq};
    use serde_json;
    use risk::BoxReport;
    use std::str::from_utf8;
    use std::fmt;

    #[test]
    fn facade_forward_starting_european_price() {

        let pricer_factory = pricer_factory_from_json(
            &mut Cursor::new(sample_pricer_factory_json())).unwrap();

        let european = instrument_from_json(
            &mut Cursor::new(sample_forward_european_json()),
            DedupControl::WriteOnce, &[],
            DedupControl::WriteOnce, &[]).unwrap();

        let market_data = market_data_from_json(
            &mut Cursor::new(sample_market_data_json())).unwrap();
        
        let fixing_table = fixing_table_from_json(
            &mut Cursor::new(sample_fixing_table_json())).unwrap();

        let delta_gamma = report_generator_from_json(
            &mut Cursor::new(sample_report_generator_json())).unwrap();

        let mut buffer = Vec::new();
        let price = calculate(pricer_factory, european, fixing_table, market_data,
            &vec![delta_gamma], true, &mut Cursor::new(&mut buffer)).unwrap();

        assert_approx(price, 3.7505621830857376, 1e-12);

        // compare the results with a hard-coded JSON result
        assert_approx_json(&buffer, sample_results_json(), 1e-12, 1e-12, "");
    }

    pub struct Fmt<F>(pub F) where F: Fn(&mut fmt::Formatter) -> fmt::Result;

    impl<F> fmt::Display for Fmt<F>
        where F: Fn(&mut fmt::Formatter) -> fmt::Result
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            (self.0)(f)
        }
    }

    fn assert_approx_json(buffer: &[u8], expected: &[u8], tol_price: f64, tol_risk: f64, msg: &str) {
        let output = from_utf8(&buffer).unwrap();
        print!("{}", output);

        let results: Vec<BoxReport> = serde_json::from_slice(&buffer).unwrap();
        let baseline: Vec<BoxReport> = serde_json::from_slice(expected).unwrap();

        let diffs = format!("{}", Fmt(|f| results.validate(&baseline, tol_price, tol_risk, msg, f)));

        assert!(diffs.is_empty(), "{}", diffs);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }

    fn sample_forward_european_json() -> &'static [u8] {
        br###"{
  "ForwardStartingEuropean": {
    "id": "SampleEuropean",
    "credit_id": "OPT",
    "underlying": {
      "Equity": {
        "id": "BP.L",
        "credit_id": "LSE",
        "currency": {
          "id": "GBP",
          "settlement": {
            "BusinessDays": {
              "calendar": {
                "WeekdayCalendar": []
              },
              "step": 2,
              "slip_forward": true
            }
          }
        },
        "settlement": {
          "BusinessDays": {
            "calendar": {
              "WeekdayCalendar": []
            },
            "step": 2,
            "slip_forward": true
          }
        }
      }
    },
    "settlement": {
      "BusinessDays": {
        "calendar": {
          "WeekdayCalendar": []
        },
        "step": 2,
        "slip_forward": true
      }
    },
    "expiry": {
      "date": "2018-12-01",
      "time_of_day": "Close"
    },
    "put_or_call": "Call",
    "cash_or_physical": "Cash",
    "expiry_time": {
      "date": "2018-12-01",
      "day_fraction": 0.8
    },
    "pay_date": "2018-12-05",
    "strike_fraction": 1.15170375,
    "strike_date": {
      "date": "2018-06-08",
      "time_of_day": "Close"
    },
    "strike_time": {
      "date": "2018-06-08",
      "day_fraction": 0.8
    }
  }
}"###
    }

    fn sample_market_data_json() -> &'static [u8] {
        br###"{
  "spot_date": "2017-01-02",
  "spots": {
    "BP.L": 100.0
  },
  "yield_curves": {
    "OPT": {
      "RateCurveAct365": {
        "base": "2016-12-30",
        "interp": {
          "left": "Flat",
          "right": "Flat",
          "points": [
            [
              "2016-12-30",
              0.05
            ],
            [
              "2017-01-13",
              0.08
            ],
            [
              "2017-06-30",
              0.09
            ],
            [
              "2017-12-29",
              0.085
            ],
            [
              "2018-12-28",
              0.082
            ]
          ]
        }
      }
    },
    "LSE": {
      "RateCurveAct365": {
        "base": "2016-12-30",
        "interp": {
          "left": "Flat",
          "right": "Flat",
          "points": [
            [
              "2016-12-30",
              0.05
            ],
            [
              "2017-01-13",
              0.08
            ],
            [
              "2017-06-30",
              0.09
            ],
            [
              "2017-12-29",
              0.085
            ],
            [
              "2018-12-28",
              0.082
            ]
          ]
        }
      }
    }
  },
  "borrow_curves": {
    "BP.L": {
      "RateCurveAct365": {
        "base": "2016-12-30",
        "interp": {
          "left": "Flat",
          "right": "Flat",
          "points": [
            [
              "2016-12-30",
              0.01
            ],
            [
              "2017-07-14",
              0.012
            ],
            [
              "2017-12-29",
              0.0125
            ],
            [
              "2018-12-28",
              0.012
            ]
          ]
        }
      }
    }
  },
  "dividends": {
    "BP.L": {
      "dividends": [
        {
          "cash": 1.2,
          "relative": 0.0,
          "ex_date": "2017-01-30",
          "pay_date": "2017-02-01"
        },
        {
          "cash": 0.8,
          "relative": 0.002,
          "ex_date": "2017-07-31",
          "pay_date": "2017-08-02"
        },
        {
          "cash": 0.2,
          "relative": 0.008,
          "ex_date": "2018-01-29",
          "pay_date": "2018-01-31"
        },
        {
          "cash": 0.0,
          "relative": 0.01,
          "ex_date": "2018-07-30",
          "pay_date": "2018-08-01"
        }
      ],
      "div_yield": {
        "RateCurveAct365": {
          "base": "2019-01-02",
          "interp": {
            "left": "Zero",
            "right": "Flat",
            "points": [
              [
                "2019-01-02",
                0.002
              ],
              [
                "2020-01-02",
                0.004
              ],
              [
                "2022-01-01",
                0.01
              ],
              [
                "2026-12-31",
                0.015
              ]
            ]
          }
        }
      },
      "last_cash_ex_date": "2018-01-29"
    }
  },
  "vol_surfaces": {
    "BP.L": {
      "FlatVolSurface": {
        "vol": 0.3,
        "calendar": {
          "WeekdayCalendar": []
        },
        "base_date": {
          "date": "2016-12-30",
          "day_fraction": 0.2
        }
      }
    }
  }
}"###
    }

    fn sample_fixing_table_json() -> &'static [u8] {
        br###"{
  "fixings_known_until": "2018-01-01",
  "fixings_by_id": {
    "BP.L": {
      "fixing_by_date": [
        [
          {
            "date": "2017-12-25",
            "time_of_day": "Close"
          },
          123.3
        ],
        [
          {
            "date": "2017-12-23",
            "time_of_day": "Open"
          },
          123.1
        ],
        [
          {
            "date": "2018-01-01",
            "time_of_day": "Open"
          },
          123.4
        ],
        [
          {
            "date": "2017-12-25",
            "time_of_day": "Open"
          },
          123.2
        ]
      ]
    }
  }
}"###
    }

    fn sample_report_generator_json() -> &'static [u8] {
        br###"{
  "DeltaGammaReportGenerator": {
    "bumpsize": 0.01
  }
}"###
    }

    fn sample_pricer_factory_json() -> &'static [u8] {
        br###"{
  "SelfPricerFactory": {}
}"###
    }

    fn sample_results_json() -> &'static [u8] {
        br###"[
  {
    "DeltaGammaReport": {
      "bumpsize": 0.01,
      "results": {
        "BP.L": {
          "delta": 0.038328385673875,
          "gamma": 0.0
        }
      }
    }
  }
]"###
    }
}