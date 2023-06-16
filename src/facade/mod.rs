pub mod c_interface;
pub mod handle;

use crate::core::dedup::{dedup_map_from_slice, Dedup, DedupControl};
use crate::core::factories::Qrc;
use crate::core::qm;
use crate::data::fixings::RcFixingTable;
use crate::instruments::assets::{Currency, RcCurrency, DEDUP_CURRENCY};
use crate::instruments::{Instrument, RcInstrument, DEDUP_INSTRUMENT};
use crate::math::numerics::ApproxEq;
use crate::pricers::RcPricerFactory;
use crate::risk::marketdata::RcMarketData;
use crate::risk::ReportTolerances;
use crate::risk::{BoxReport, RcReportGenerator};
use serde::Deserialize;
use serde::Serialize;
use serde_json as sdj;
use std::fmt;
use std::io::{Read, Write};
use std::sync::Arc;

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
pub fn instrument_from_json(
    source: &mut dyn Read,
    ccy_dedup: DedupControl,
    currencies: &[RcCurrency],
    instr_dedup: DedupControl,
    instruments: &[RcInstrument],
) -> Result<RcInstrument, qm::Error> {
    // create a deserializer based on the input
    let mut deserializer = sdj::Deserializer::from_reader(source);

    // convert the currencies and instrument components to maps, for efficient deduplication
    let currencies_map = dedup_map_from_slice(currencies);
    let instruments_map = dedup_map_from_slice(instruments);

    let mut ccy = Dedup::<Currency, Arc<Currency>>::new(ccy_dedup, currencies_map);
    let mut opt = Dedup::<dyn Instrument, Qrc<dyn Instrument>>::new(instr_dedup, instruments_map);
    let instr = ccy.with(&DEDUP_CURRENCY, || {
        opt.with(&DEDUP_INSTRUMENT, || {
            RcInstrument::deserialize(&mut deserializer)
        })
    })?;
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
pub fn currency_from_json(source: &mut dyn Read) -> Result<RcCurrency, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the currency
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let currency = RcCurrency::deserialize(&mut deserializer)?;
    Ok(currency)
}

/// A PricerFactory is used to create pricers, given an instrument to price and a fixing table.
/// It contains enough information to define how the pricing is done, for example whether it is
/// Monte-Carlo, how many paths, and which model to use.
pub fn pricer_factory_from_json(source: &mut dyn Read) -> Result<RcPricerFactory, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let pricer_factory = RcPricerFactory::deserialize(&mut deserializer)?;
    Ok(pricer_factory)
}

/// A FixingTable contains historical fixings for one or more assets, defined by Date and time of
/// day.
pub fn fixing_table_from_json(source: &mut dyn Read) -> Result<RcFixingTable, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let fixing_table = RcFixingTable::deserialize(&mut deserializer)?;
    Ok(fixing_table)
}

/// MarketData contains all the live market data required for pricing. This is spots, yield
/// and borrow curves, dividends, volatilities, and correlations. You can instantiate a
/// MarketData as here by deserializing it from json. It may be more efficient to instantiate
/// it directly, by using methods in the data module.
pub fn market_data_from_json(source: &mut dyn Read) -> Result<RcMarketData, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let market_data = RcMarketData::deserialize(&mut deserializer)?;
    Ok(market_data)
}

/// A ReportGenerator takes a pricer and bumps and revalues it to generate a report
/// of sensitivities to market data changes.
pub fn report_generator_from_json(source: &mut dyn Read) -> Result<RcReportGenerator, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let report_generator = RcReportGenerator::deserialize(&mut deserializer)?;
    Ok(report_generator)
}

/// Reports are normally generated by the calculate function, but this method allows us to
/// reconstitute a vector of reports from its json representation, for example for testing
/// or distribution.
pub fn reports_from_json(source: &mut dyn Read) -> Result<Vec<BoxReport>, qm::Error> {
    // create a deserializer based on the input and use it to deserialize the pricer factory
    let mut deserializer = sdj::Deserializer::from_reader(source);
    let reports = Vec::<BoxReport>::deserialize(&mut deserializer)?;
    Ok(reports)
}

/// Performs a calculation, outputting the price, and writing any reports that are
/// requested.
pub fn calculate(
    pricer_factory: RcPricerFactory,
    instrument: RcInstrument,
    fixing_table: RcFixingTable,
    market_data: RcMarketData,
    report_generators: &[RcReportGenerator],
) -> Result<Vec<BoxReport>, qm::Error> {
    let mut pricer = pricer_factory.new(instrument, fixing_table, market_data)?;
    let price = pricer.price()?;
    let mut saveable = pricer.as_bumpable().new_saveable();

    let mut reports = Vec::with_capacity(report_generators.len());
    for report_generator in report_generators.iter() {
        let report = report_generator.generate(&mut *pricer, &mut *saveable, price)?;
        reports.push(report);
    }

    Ok(reports)
}

/// Unpacks a set of calculation results to the given stream. For example, they may be
/// written to a string buffer or to a file.
pub fn write_results(
    reports: &[BoxReport],
    pretty: bool,
    out: &mut dyn Write,
) -> Result<(), qm::Error> {
    serialize_output(reports, pretty, out)
}

pub struct Fmt<F>(pub F)
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result;

impl<F> fmt::Display for Fmt<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(f)
    }
}

/// Checks that the two supplied JSON strings are sufficiently close, given the tolerances.
/// The strings should contain vectors of reports.
pub fn assert_approx_eq_reports(
    reports: &[BoxReport],
    expected: &[BoxReport],
    tol_price: f64,
    tol_ccy_risk: f64,
    tol_unit_risk: f64,
) -> Result<(), qm::Error> {
    let tolerances = ReportTolerances::new(tol_price, tol_ccy_risk, tol_unit_risk);
    let diffs = format!(
        "{}",
        Fmt(|f| reports.validate(expected, &tolerances, "", f))
    );

    if diffs.is_empty() {
        Ok(())
    } else {
        Err(qm::Error::new(&format!("Mismatch: {}", diffs)))
    }
}

fn serialize_output<T>(to_write: &T, pretty: bool, out: &mut dyn Write) -> Result<(), qm::Error>
where
    T: Serialize + ?Sized,
{
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
pub mod tests {
    use super::*;
    use std::io::Cursor;
    use std::str::from_utf8;

    #[test]
    fn facade_forward_starting_european_price() {
        let pricer_factory =
            pricer_factory_from_json(&mut Cursor::new(sample_pricer_factory_json())).unwrap();

        let european = instrument_from_json(
            &mut Cursor::new(sample_forward_european_json()),
            DedupControl::WriteOnce,
            &[],
            DedupControl::WriteOnce,
            &[],
        )
        .unwrap();

        let market_data =
            market_data_from_json(&mut Cursor::new(sample_market_data_json())).unwrap();

        let fixing_table =
            fixing_table_from_json(&mut Cursor::new(sample_fixing_table_json())).unwrap();

        let delta_gamma =
            report_generator_from_json(&mut Cursor::new(sample_report_generator_json())).unwrap();

        let reports = calculate(
            pricer_factory,
            european,
            fixing_table,
            market_data,
            &[delta_gamma],
        )
        .unwrap();

        let mut buffer = Vec::new();
        write_results(&reports, true, &mut Cursor::new(&mut buffer)).unwrap();

        // write the output to stdout, in case we mismatch (makes it easier to update the
        // baseline if that is what we want to do)
        let output = from_utf8(&buffer).unwrap();
        print!("{}", output);

        // compare the results with a hard-coded JSON result
        let results: Vec<BoxReport> = sdj::from_slice(&buffer).unwrap();
        let baseline: Vec<BoxReport> = sdj::from_slice(sample_results_json()).unwrap();

        assert_approx_eq_reports(&results, &baseline, 1e-12, 1e-12, 1e-12).unwrap();
    }

    #[test]
    fn facade_read_currency() {
        let _ = currency_from_json(&mut Cursor::new(sample_currency_json())).unwrap();
    }

    #[test]
    fn facade_read_equity() {
        let currency = currency_from_json(&mut Cursor::new(sample_currency_json())).unwrap();

        let _ = instrument_from_json(
            &mut Cursor::new(sample_equity_json()),
            DedupControl::WriteOnce,
            &[currency],
            DedupControl::WriteOnce,
            &[],
        )
        .unwrap();
    }

    pub fn sample_currency_json() -> &'static [u8] {
        br###"{
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
}"###
    }

    pub fn sample_equity_json() -> &'static [u8] {
        br###"{
    "Equity": {
        "id": "AZ.L",
        "credit_id": "LSE",
        "currency": "GBP",
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
}"###
    }

    pub fn sample_forward_european_json() -> &'static [u8] {
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

    pub fn sample_market_data_json() -> &'static [u8] {
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

    pub fn sample_fixing_table_json() -> &'static [u8] {
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

    pub fn sample_report_generator_json() -> &'static [u8] {
        br###"{
  "DeltaGammaReportGenerator": {
    "bumpsize": 0.01
  }
}"###
    }

    pub fn sample_pricer_factory_json() -> &'static [u8] {
        br###"{
  "SelfPricerFactory": {}
}"###
    }

    pub fn sample_results_json() -> &'static [u8] {
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
