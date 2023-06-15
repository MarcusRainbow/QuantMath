extern crate quantmath as qm;

use qm::core::{factories::Qrc};
use qm::data::{
    bump::Bump,
    bumpdivs::BumpDivs,
    bumpspot::BumpSpot,
    bumpvol::BumpVol,
    bumpyield::BumpYield,
    curves::{RateCurveAct365, RcRateCurve},
    divstream::{Dividend, DividendStream, RcDividendStream},
    fixings::{FixingTable, RcFixingTable},
    volsurface::{FlatVolSurface, RcVolSurface},
};
use qm::dates::{
    calendar::{RcCalendar, WeekdayCalendar},
    datetime::{DateDayFraction, DateTime, TimeOfDay},
    rules::{BusinessDays, RcDateRule},
    Date,
};
use qm::instruments::{
    assets::{Currency, Equity, RcCurrency},
    options::{OptionSettlement, PutOrCall, SpotStartingEuropean},
    Priceable, RcInstrument,
};
use qm::math::{interpolation::Extrap, numerics::approx_eq};
use qm::models::{blackdiffusion::BlackDiffusionFactory, RcMonteCarloModelFactory};
use qm::pricers::{
    montecarlo::{MonteCarloPricerFactory}, //, RcMonteCarloModelFactory},
    PricerFactory,
};
use qm::risk::marketdata::{MarketData, RcMarketData};
use std::collections::HashMap;
use std::sync::Arc;

fn create_sample_divstream() -> RcDividendStream {
    // Early divs are purely cash. Later ones are mixed cash/relative
    let d = Date::from_ymd(2017, 1, 2);
    let divs = [
        Dividend::new(1.2, 0.0, d + 28, d + 30),
        Dividend::new(0.8, 0.002, d + 210, d + 212),
        Dividend::new(0.2, 0.008, d + 392, d + 394),
        Dividend::new(0.0, 0.01, d + 574, d + 576),
    ];

    // dividend yield for later-dated divs. Note that the base date
    // for the curve is after the last of the explicit dividends.
    let points = [
        (d + 365 * 2, 0.002),
        (d + 365 * 3, 0.004),
        (d + 365 * 5, 0.01),
        (d + 365 * 10, 0.015),
    ];
    let curve = RateCurveAct365::new(d + 365 * 2, &points, Extrap::Zero, Extrap::Flat).unwrap();
    let div_yield = RcRateCurve::new(Arc::new(curve));

    RcDividendStream::new(Arc::new(DividendStream::new(&divs, div_yield)))
}

fn create_sample_rate() -> RcRateCurve {
    let d = Date::from_ymd(2016, 12, 30);
    let rate_points = [
        (d, 0.05),
        (d + 14, 0.08),
        (d + 182, 0.09),
        (d + 364, 0.085),
        (d + 728, 0.082),
    ];
    RcRateCurve::new(Arc::new(
        RateCurveAct365::new(d, &rate_points, Extrap::Flat, Extrap::Flat).unwrap(),
    ))
}

fn create_sample_borrow() -> RcRateCurve {
    let d = Date::from_ymd(2016, 12, 30);
    let borrow_points = [
        (d, 0.01),
        (d + 196, 0.012),
        (d + 364, 0.0125),
        (d + 728, 0.012),
    ];
    RcRateCurve::new(Arc::new(
        RateCurveAct365::new(d, &borrow_points, Extrap::Flat, Extrap::Flat).unwrap(),
    ))
}

fn create_sample_flat_vol() -> RcVolSurface {
    let calendar = RcCalendar::new(Arc::new(WeekdayCalendar()));
    let base_date = Date::from_ymd(2016, 12, 30);
    let base = DateDayFraction::new(base_date, 0.2);
    RcVolSurface::new(Arc::new(FlatVolSurface::new(0.3, calendar, base)))
}

fn sample_currency(step: u32) -> Currency {
    let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
    let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)));
    Currency::new("GBP", settlement)
}

fn sample_settlement(step: u32) -> RcDateRule {
    let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
    RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)))
}

fn sample_equity(currency: RcCurrency, step: u32) -> Equity {
    let settlement = sample_settlement(step);
    Equity::new("BP.L", "LSE", currency, settlement)
}

fn sample_european() -> Arc<SpotStartingEuropean> {
    let strike = 100.0;
    let put_or_call = PutOrCall::Call;
    let expiry = DateTime::new(Date::from_ymd(2018, 6, 1), TimeOfDay::Close);
    let currency = RcCurrency::new(Arc::new(sample_currency(2)));
    let settlement = sample_settlement(2);
    let equity = RcInstrument::new(Qrc::new(Arc::new(sample_equity(currency, 2))));
    let european = SpotStartingEuropean::new(
        "SampleSpotEuropean",
        "OPT",
        equity.clone(),
        settlement,
        expiry,
        strike,
        put_or_call,
        OptionSettlement::Cash,
    )
    .unwrap();
    Arc::new(european)
}

fn sample_fixings() -> FixingTable {
    let today = Date::from_ymd(2017, 1, 2);
    FixingTable::from_fixings(
        today,
        &[(
            "BP.L",
            &[(DateTime::new(today - 7, TimeOfDay::Close), 102.0)],
        )],
    )
    .unwrap()
}

fn assert_approx(value: f64, expected: f64, tolerance: f64) {
    assert!(
        approx_eq(value, expected, tolerance),
        "value={} expected={}",
        value,
        expected
    );
}

fn main() {
    // prepare dummy market data

    let market_data: RcMarketData = RcMarketData::new(Arc::new({
        let spot_date = Date::from_ymd(2017, 1, 2);

        let mut spots = HashMap::new();
        spots.insert("BP.L".to_string(), 100.0);
        spots.insert("GSK.L".to_string(), 200.0);

        let mut dividends = HashMap::new();
        dividends.insert("BP.L".to_string(), create_sample_divstream());
        dividends.insert("GSK.L".to_string(), create_sample_divstream());

        let mut yield_curves = HashMap::new();
        yield_curves.insert("OPT".to_string(), create_sample_rate());
        yield_curves.insert("LSE".to_string(), create_sample_rate());

        let mut borrow_curves = HashMap::new();
        borrow_curves.insert("BP.L".to_string(), create_sample_borrow());
        borrow_curves.insert("GSK.L".to_string(), create_sample_borrow());

        let mut vol_surfaces = HashMap::new();
        vol_surfaces.insert("BP.L".to_string(), create_sample_flat_vol());
        vol_surfaces.insert("GSK.L".to_string(), create_sample_flat_vol());

        MarketData::new(
            spot_date,
            spots,
            yield_curves,
            borrow_curves,
            dividends,
            vol_surfaces,
        )
    }));

    let instrument = RcInstrument::new(Qrc::new(sample_european()));
    let fixings = RcFixingTable::new(Arc::new(sample_fixings()));

    let n_paths = 100000;
    let correlation_substep = 20;
    let path_substep = 0.01;
    let model_factory = RcMonteCarloModelFactory::new(Arc::new(BlackDiffusionFactory::new(
        correlation_substep,
        path_substep,
        n_paths,
    )));
    let factory = MonteCarloPricerFactory::new(model_factory);
    let mut pricer = factory.new(instrument, fixings, market_data).unwrap();
    let mut save = pricer.as_bumpable().new_saveable();

    let unbumped_price = pricer.price().unwrap();
    assert_approx(unbumped_price, 16.710717400832973, 0.3);

    // now bump the spot and price. Note that this equates to roughly
    // delta of 0.5, which is what we expect for an atm option
    let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
    let bumped = pricer
        .as_mut_bumpable()
        .bump(&bump, Some(&mut *save))
        .unwrap();
    assert!(bumped);
    let bumped_price = pricer.price().unwrap();
    assert_approx(bumped_price - unbumped_price, 0.633187905501792, 0.02);

    // when we restore, it should take the price back
    pricer.as_mut_bumpable().restore(&*save).unwrap();
    save.clear();
    let price = pricer.price().unwrap();
    assert_approx(price, unbumped_price, 1e-12);

    // now bump the vol and price. The new price is a bit larger, as
    // expected. (An atm option has roughly max vega.)
    let bump = Bump::new_vol("BP.L", BumpVol::new_flat_additive(0.01));
    let bumped = pricer
        .as_mut_bumpable()
        .bump(&bump, Some(&mut *save))
        .unwrap();
    assert!(bumped);
    let bumped_price = pricer.price().unwrap();
    assert_approx(bumped_price - unbumped_price, 0.429105019892687, 0.02);

    // when we restore, it should take the price back
    pricer.as_mut_bumpable().restore(&*save).unwrap();
    save.clear();
    let price = pricer.price().unwrap();
    assert_approx(price, unbumped_price, 1e-12);

    // now bump the divs and price. As expected, this makes the
    // price decrease by a small amount.
    let bump = Bump::new_divs("BP.L", BumpDivs::new_all_relative(0.01));
    let bumped = pricer
        .as_mut_bumpable()
        .bump(&bump, Some(&mut *save))
        .unwrap();
    assert!(bumped);
    let bumped_price = pricer.price().unwrap();
    assert_approx(bumped_price - unbumped_price, -0.01968507722361, 0.001);

    // when we restore, it should take the price back
    pricer.as_mut_bumpable().restore(&*save).unwrap();
    save.clear();
    let price = pricer.price().unwrap();
    assert_approx(price, unbumped_price, 1e-12);

    // now bump the yield underlying the equity and price. This
    // increases the forward, so we expect the call price to increase.
    let bump = Bump::new_yield("LSE", BumpYield::new_flat_annualised(0.01));
    let bumped = pricer
        .as_mut_bumpable()
        .bump(&bump, Some(&mut *save))
        .unwrap();
    assert!(bumped);
    let bumped_price = pricer.price().unwrap();
    assert_approx(bumped_price - unbumped_price, 0.814646953109683, 0.01);

    // when we restore, it should take the price back
    pricer.as_mut_bumpable().restore(&*save).unwrap();
    save.clear();
    let price = pricer.price().unwrap();
    assert_approx(price, unbumped_price, 1e-12);

    // now bump the yield underlying the option and price
    let bump = Bump::new_yield("OPT", BumpYield::new_flat_annualised(0.01));
    let bumped = pricer
        .as_mut_bumpable()
        .bump(&bump, Some(&mut *save))
        .unwrap();
    assert!(bumped);
    let bumped_price = pricer.price().unwrap();
    assert_approx(bumped_price - unbumped_price, -0.215250594911648, 0.01);

    // when we restore, it should take the price back
    pricer.as_mut_bumpable().restore(&*save).unwrap();
    save.clear();
    let price = pricer.price().unwrap();
    assert_approx(price, unbumped_price, 1e-12);
}
