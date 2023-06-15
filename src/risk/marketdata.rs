use crate::core::qm;
use crate::data::bump::Bump;
use crate::data::bump::Bumper;
use crate::data::bumpdivs::BumpDivs;
use crate::data::bumpspot::BumpSpot;
use crate::data::bumpspotdate::BumpSpotDate;
use crate::data::bumpspotdate::SpotDynamics;
use crate::data::bumpvol::BumpVol;
use crate::data::bumpyield::BumpYield;
use crate::data::curves::RcRateCurve;
use crate::data::divstream::RcDividendStream;
use crate::data::forward::EquityForward;
use crate::data::forward::Forward;
use crate::data::volsurface::RcVolSurface;
use crate::dates::Date;
use crate::instruments::Instrument;
use crate::instruments::PricingContext;
use crate::risk::dependencies::DependencyCollector;
use crate::risk::Bumpable;
use crate::risk::BumpablePricingContext;
use crate::risk::Saveable;
use serde as sd;
use std::any::Any;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

/// The market data struct contains all the market data supplied for a
/// valuation. It has methods for building the analytics needed for valuation
/// such as forwards.
///
/// All market data is identified by a single string. Where data should be
/// keyed by multiple fields, for example a yield curve is keyed by currency
/// and credit entity, there is a conventional way of combining the ids of
/// the fields to create a unique key.
///
/// As new forms of market data are required, they should be added to this
/// struct. They may also need to be added to PricingContext, so they can be
/// accessed during pricing.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MarketData {
    spot_date: Date,
    spots: HashMap<String, f64>,
    yield_curves: HashMap<String, RcRateCurve>,
    borrow_curves: HashMap<String, RcRateCurve>,
    dividends: HashMap<String, RcDividendStream>,
    vol_surfaces: HashMap<String, RcVolSurface>,
}

impl MarketData {
    /// Creates a market data object. There is normally only one of these
    /// supplied to any valuation.
    ///
    /// Apart from spots, all market data is allowed to have a base date in
    /// the past, in which case standard rules are used for bringing the values
    /// up to date. This is useful for valuation early in the morning, before
    /// the market has opened to give liquid option prices etc.
    ///
    /// * 'spot_date'      - The date of all the spot values. Normally today
    /// * 'spots'          - Values of any numeric screen prices, keyed by the
    ///                      id of the instrument, such as an equity
    /// * 'yield_curves'   - Precooked yield curves, keyed by credit id
    /// * 'borrow_curves'  - Cost of borrow or repo curves, keyed by the id
    ///                      of the instrument, such as an equity
    /// * 'dividends'      - Dividend streams, keyed by the id of the equity
    /// * 'vol_surfaces'   - Vol surfaces, keyed by the id of the instrument
    ///                      such as an equity. Vol cubes for interest rates
    ///                      will be supplied as a separate entry.
    pub fn new(
        spot_date: Date,
        spots: HashMap<String, f64>,
        yield_curves: HashMap<String, RcRateCurve>,
        borrow_curves: HashMap<String, RcRateCurve>,
        dividends: HashMap<String, RcDividendStream>,
        vol_surfaces: HashMap<String, RcVolSurface>,
    ) -> MarketData {
        MarketData {
            spot_date: spot_date,
            spots: spots,
            yield_curves: yield_curves,
            borrow_curves: borrow_curves,
            dividends: dividends,
            vol_surfaces: vol_surfaces,
        }
    }

    /// Bumps the spot date, for example during a Theta calculation
    pub fn bump_spot_date(
        &mut self,
        bump: &BumpSpotDate,
        dependencies: &DependencyCollector,
    ) -> Result<(), qm::Error> {
        let new_spot_date = bump.spot_date();

        match bump.spot_dynamics() {
            SpotDynamics::StickyForward => self.sticky_forward_bump(new_spot_date, dependencies)?,
            SpotDynamics::StickySpot => self.sticky_spot_bump(new_spot_date, dependencies)?,
        }

        self.spot_date = new_spot_date;
        Ok(())
    }

    fn sticky_forward_bump(
        &mut self,
        new_spot_date: Date,
        dependencies: &DependencyCollector,
    ) -> Result<(), qm::Error> {
        // We cannot modify the spots map in situ because it is also being used for the
        // forward curve calculations (borrow rules in Rust). Rather than introduce unsafe
        // code, we just do it by creating a new HashMap and then assigning it.
        let mut new_spots = HashMap::<String, f64>::new();

        for (id, spot) in self.spots.iter() {
            if let Some(instrument) = dependencies.instrument_by_id(id) {
                let instr: &Instrument = &*instrument.clone();
                let curve = self.forward_curve(instr, new_spot_date)?;
                let new_spot = curve.forward(new_spot_date)?;
                new_spots.insert(id.to_string(), new_spot);
            } else {
                // If we cannot find the instrument, the most likely reason
                // is that the user has supplied market data for it but the
                // instruments either do not require it or only require spot.
                // For now, just assume spot dynamics
                new_spots.insert(id.to_string(), *spot);
            }
        }

        self.spots = new_spots;

        Ok(())
    }

    fn sticky_spot_bump(
        &mut self,
        _new_spot_date: Date,
        _dependencies: &DependencyCollector,
    ) -> Result<(), qm::Error> {
        // for now, do nothing. Need to think about what to do about dividends
        Ok(())
    }
}

/// Create a new type for a Rc<MarketData> so we can implement serialize
/// and deserialize functions for it.
#[derive(Clone, Debug)]
pub struct RcMarketData(Arc<MarketData>);

impl RcMarketData {
    pub fn new(table: Arc<MarketData>) -> RcMarketData {
        RcMarketData(table)
    }
}

impl Deref for RcMarketData {
    type Target = MarketData;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl sd::Serialize for RcMarketData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
    {
        self.0.serialize(serializer)
    }
}

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for RcMarketData {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: sd::Deserializer<'de>,
    {
        let md = MarketData::deserialize(deserializer)?;
        Ok(RcMarketData::new(Arc::new(md)))
    }
}

impl PricingContext for MarketData {
    fn spot_date(&self) -> Date {
        self.spot_date
    }

    fn yield_curve(
        &self,
        credit_id: &str,
        _high_water_mark: Date,
    ) -> Result<RcRateCurve, qm::Error> {
        find_market_data(credit_id, &self.yield_curves, "Yield curve")
    }

    fn spot(&self, id: &str) -> Result<f64, qm::Error> {
        find_market_data(id, &self.spots, "Spot")
    }

    fn forward_curve(
        &self,
        instrument: &Instrument,
        high_water_mark: Date,
    ) -> Result<Arc<Forward>, qm::Error> {
        // This assumes the instrument is an equity. Need handling for other
        // types of underlying that may not have dividends or borrow, or may
        // be driftless
        let id = instrument.id();
        let spot = find_market_data(id, &self.spots, "Spot")?;
        let divs = find_market_data(id, &self.dividends, "Dividends")?;
        let borrow = find_market_data(id, &self.borrow_curves, "Borrow curve")?;

        let credit_id = instrument.credit_id();
        let yield_curve =
            find_market_data(&credit_id, &self.yield_curves, "Yield curve for forward")?;

        // We create the forward on the fly. For efficiency, we could cache
        // the forward if the request is the same and there are no relevant
        // bumps
        let settlement = instrument.settlement().clone();
        let forward = EquityForward::new(
            self.spot_date,
            spot,
            settlement,
            yield_curve,
            borrow,
            &*divs,
            high_water_mark,
        )?;
        Ok(Arc::new(forward))
    }

    /// Gets a Vol Surface, given any instrument, for example an equity.  Also
    /// specify a high water mark, beyond which we never directly ask for
    /// vols.
    fn vol_surface(
        &self,
        instrument: &Instrument,
        _high_water_mark: Date,
        forward_fn: &Fn() -> Result<Arc<Forward>, qm::Error>,
    ) -> Result<RcVolSurface, qm::Error> {
        let id = instrument.id();
        let mut vol = find_market_data(id, &self.vol_surfaces, "Vol surface")?;

        // decorate or modify the surface to cope with any time or forward shift
        instrument
            .vol_time_dynamics()
            .modify(&mut vol, self.spot_date)?;
        instrument
            .vol_forward_dynamics()
            .modify(&mut vol, forward_fn)?;
        Ok(vol)
    }

    fn correlation(&self, _first: &Instrument, _second: &Instrument) -> Result<f64, qm::Error> {
        Err(qm::Error::new("Correlation not implemented"))
    }
}

fn find_market_data<T: Clone>(
    id: &str,
    collection: &HashMap<String, T>,
    item: &str,
) -> Result<T, qm::Error> {
    match collection.get(id) {
        None => Err(qm::Error::new(&format!("{} not found: '{}'", item, id))),
        Some(x) => Ok(x.clone()),
    }
}

impl Bumpable for MarketData {
    fn bump(&mut self, bump: &Bump, save: Option<&mut Saveable>) -> Result<bool, qm::Error> {
        let saved = to_saved_data(save)?;

        // Throughout this code, we need to cast the bump to the specific type
        // e.g. bump as &BumpSpot, even though it already is this type. I think
        // this is a compiler bug.
        match bump {
            &Bump::Spot(ref id, ref bump) => apply_bump(
                &id,
                bump as &BumpSpot,
                &mut self.spots,
                saved.map_or(None, |s| Some(&mut s.spots)),
            ),
            &Bump::Divs(ref id, ref bump) => apply_bump(
                &id,
                bump as &BumpDivs,
                &mut self.dividends,
                saved.map_or(None, |s| Some(&mut s.dividends)),
            ),
            &Bump::Borrow(ref id, ref bump) => apply_bump(
                &id,
                bump as &BumpYield,
                &mut self.borrow_curves,
                saved.map_or(None, |s| Some(&mut s.borrow_curves)),
            ),
            &Bump::Vol(ref id, ref bump) => apply_bump(
                &id,
                bump as &BumpVol,
                &mut self.vol_surfaces,
                saved.map_or(None, |s| Some(&mut s.vol_surfaces)),
            ),
            &Bump::Yield(ref credit_id, ref bump) => apply_bump(
                &credit_id,
                bump as &BumpYield,
                &mut self.yield_curves,
                saved.map_or(None, |s| Some(&mut s.yield_curves)),
            ),
            &Bump::SpotDate(_) => Err(qm::Error::new(
                "MarketData does not have \
                enough information to handle spot date bumping on its own. It needs \
                to be handled by a containing PricingContextPrefetch.",
            )),
        }
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        Err(qm::Error::new("Dependency information not available"))
    }

    fn new_saveable(&self) -> Box<Saveable> {
        Box::new(SavedData::new())
    }

    fn context(&self) -> &PricingContext {
        self.as_pricing_context()
    }

    fn restore(&mut self, any_saved: &Saveable) -> Result<(), qm::Error> {
        if let Some(saved) = any_saved.as_any().downcast_ref::<SavedData>() {
            copy_from_saved(&mut self.spots, &saved.spots);
            copy_from_saved(&mut self.yield_curves, &saved.yield_curves);
            copy_from_saved(&mut self.borrow_curves, &saved.borrow_curves);
            copy_from_saved(&mut self.dividends, &saved.dividends);
            copy_from_saved(&mut self.vol_surfaces, &saved.vol_surfaces);
            Ok(())
        } else {
            Err(qm::Error::new("Mismatching save space for restore"))
        }
    }
}

impl BumpablePricingContext for MarketData {
    fn as_bumpable(&self) -> &Bumpable {
        self
    }
    fn as_mut_bumpable(&mut self) -> &mut Bumpable {
        self
    }
    fn as_pricing_context(&self) -> &PricingContext {
        self
    }
    fn raw_market_data(&self) -> &MarketData {
        self
    }
}

fn to_saved_data(opt_save: Option<&mut Saveable>) -> Result<Option<&mut SavedData>, qm::Error> {
    if let Some(save) = opt_save {
        if let Some(as_self) = save.as_mut_any().downcast_mut::<SavedData>() {
            Ok(Some(as_self))
        } else {
            Err(qm::Error::new(
                "Mismatching save space for bump market data",
            ))
        }
    } else {
        Ok(None)
    }
}

// local helper function to apply a bump and save the old state
fn apply_bump<T: Clone>(
    id: &str,
    bump: &Bumper<T>,
    to_bump: &mut HashMap<String, T>,
    to_save: Option<&mut HashMap<String, T>>,
) -> Result<bool, qm::Error> {
    // try to find the entry in the map of things to bump
    let key = id.to_string();
    if let Some(entry) = to_bump.get_mut(&key) {
        // save the old value if there is anywhere to save it
        if let Some(save) = to_save {
            save.insert(key, entry.clone());
        }

        // update the new value and return true to say we changed it
        *entry = bump.apply(entry.clone());
        Ok(true)
    } else {
        // value not found, so return false to say we did not change it
        Ok(false)
    }
}

pub fn copy_from_saved<T: Clone>(to_restore: &mut HashMap<String, T>, saved: &HashMap<String, T>) {
    for (key, value) in saved.iter() {
        to_restore.insert(key.to_string(), value.clone());
    }
}

pub struct SavedData {
    spots: HashMap<String, f64>,
    yield_curves: HashMap<String, RcRateCurve>,
    borrow_curves: HashMap<String, RcRateCurve>,
    dividends: HashMap<String, RcDividendStream>,
    vol_surfaces: HashMap<String, RcVolSurface>,
}

impl SavedData {
    /// Creates an empty market data object, which can be used for saving state
    /// so it can be restored after a bump
    pub fn new() -> SavedData {
        SavedData {
            spots: HashMap::new(),
            yield_curves: HashMap::new(),
            borrow_curves: HashMap::new(),
            dividends: HashMap::new(),
            vol_surfaces: HashMap::new(),
        }
    }
}

impl Saveable for SavedData {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_mut_any(&mut self) -> &mut Any {
        self
    }

    fn clear(&mut self) {
        self.spots.clear();
        self.yield_curves.clear();
        self.borrow_curves.clear();
        self.dividends.clear();
        self.vol_surfaces.clear();
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::factories::Qrc;
    use crate::data::bumpdivs::BumpDivs;
    use crate::data::bumpspot::BumpSpot;
    use crate::data::bumpvol::BumpVol;
    use crate::data::bumpyield::BumpYield;
    use crate::data::curves::RateCurveAct365;
    use crate::data::divstream::Dividend;
    use crate::data::divstream::DividendStream;
    use crate::data::volsurface::FlatVolSurface;
    use crate::data::volsurface::RcVolSurface;
    use crate::dates::{
        calendar::RcCalendar, calendar::WeekdayCalendar, datetime::DateDayFraction,
        datetime::DateTime, datetime::TimeOfDay, rules::BusinessDays, rules::RcDateRule,
    };
    use crate::instruments::{
        assets::Currency, assets::Equity, assets::RcCurrency, options::ForwardStartingEuropean,
        options::OptionSettlement, options::PutOrCall, options::SpotStartingEuropean, Priceable,
        RcInstrument,
    };
    use crate::math::interpolation::Extrap;
    use crate::math::numerics::approx_eq;
    use serde_json;
    use std::sync::Arc;

    pub fn sample_currency(step: u32) -> Currency {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)));
        Currency::new("GBP", settlement)
    }

    pub fn sample_settlement(step: u32) -> RcDateRule {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)))
    }

    pub fn sample_equity(currency: RcCurrency, step: u32) -> Equity {
        let settlement = sample_settlement(step);
        Equity::new("BP.L", "LSE", currency, settlement)
    }

    pub fn sample_european() -> Arc<SpotStartingEuropean> {
        let strike = 100.0;
        let put_or_call = PutOrCall::Call;
        let expiry = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
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

    pub fn sample_forward_european() -> Arc<ForwardStartingEuropean> {
        let strike_fraction = 0.95;
        let strike_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Close);
        let put_or_call = PutOrCall::Call;
        let expiry = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
        let currency = RcCurrency::new(Arc::new(sample_currency(2)));
        let settlement = sample_settlement(2);
        let equity = RcInstrument::new(Qrc::new(Arc::new(sample_equity(currency, 2))));
        let european = ForwardStartingEuropean::new(
            "SampleForwardEuropean",
            "OPT",
            equity.clone(),
            settlement,
            expiry,
            strike_fraction,
            strike_date,
            put_or_call,
            OptionSettlement::Cash,
        )
        .unwrap();
        Arc::new(european)
    }

    pub fn create_sample_divstream() -> RcDividendStream {
        // Early divs are purely cash. Later ones are mixed cash/relative
        let d = Date::from_ymd(2017, 01, 02);
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

    pub fn create_sample_rate() -> RcRateCurve {
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

    pub fn create_sample_borrow() -> RcRateCurve {
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

    pub fn create_sample_flat_vol() -> RcVolSurface {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar()));
        let base_date = Date::from_ymd(2016, 12, 30);
        let base = DateDayFraction::new(base_date, 0.2);
        RcVolSurface::new(Arc::new(FlatVolSurface::new(0.3, calendar, base)))
    }

    pub fn sample_market_data() -> MarketData {
        let spot_date = Date::from_ymd(2017, 01, 02);
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
    }

    #[test]
    fn european_unbumped_price() {
        let market_data = sample_market_data();
        let european = sample_european();
        let val_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Open);
        let price = european.price(&market_data, val_date).unwrap();

        // this price looks plausible, but was found simply by running the test
        assert_approx(price, 16.710717400832973, 1e-12);
    }

    #[test]
    fn european_bumped_price() {
        let market_data = sample_market_data();
        let european = sample_european();
        let val_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Open);
        let unbumped_price = european.price(&market_data, val_date).unwrap();

        // clone the market data so we can modify it and also create an
        // empty saved data to save state so we can restore it
        let mut mut_data = market_data.clone();
        let mut save = SavedData::new();

        // now bump the spot and price. Note that this equates to roughly
        // delta of 0.5, which is what we expect for an atm option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 17.343905306334765, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the vol and price. The new price is a bit larger, as
        // expected. (An atm option has roughly max vega.)
        let bump = Bump::new_vol("BP.L", BumpVol::new_flat_additive(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 17.13982242072566, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the divs and price. As expected, this makes the
        // price decrease by a small amount.
        let bump = Bump::new_divs("BP.L", BumpDivs::new_all_relative(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 16.691032323609356, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the yield underlying the equity and price. This
        // increases the forward, so we expect the call price to increase.
        let bump = Bump::new_yield("LSE", BumpYield::new_flat_annualised(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 17.299620299229513, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the yield underlying the option and price
        let bump = Bump::new_yield("OPT", BumpYield::new_flat_annualised(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 16.710717400832973, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);
    }

    #[test]
    fn forward_european_tests() {
        let market_data = sample_market_data();
        let european = sample_forward_european();
        let val_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Open);
        let unbumped_price = european.price(&market_data, val_date).unwrap();

        // this price looks plausible, but was found simply by running the test
        assert_approx(unbumped_price, 19.059001770739144, 1e-12);

        // clone the market data so we can modify it and also create an
        // empty saved data to save state so we can restore it
        let mut mut_data = market_data.clone();
        let mut save = SavedData::new();

        // now bump the spot and price. Note that this equates to quite small delta
        // which is what we expect for a forward-starting option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 19.264143625005346, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the vol and price. The new price is a bit larger, as
        // expected. (An atm option has roughly max vega.)
        let bump = Bump::new_vol("BP.L", BumpVol::new_flat_additive(0.01));
        let bumped = mut_data.bump(&bump, Some(&mut save)).unwrap();
        assert!(bumped);
        let bumped_price = european.price(&mut_data, val_date).unwrap();
        assert_approx(bumped_price, 19.462049109434098, 1e-12);

        // when we restore, it should take the price back
        mut_data.restore(&save).unwrap();
        save.clear();
        let price = european.price(&mut_data, val_date).unwrap();
        assert_approx(price, unbumped_price, 1e-12);
    }

    #[test]
    fn serde_market_data_roundtrip() {
        // create some sample data and price a european with it
        let market_data = sample_market_data();
        let european = sample_european();
        let val_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Open);
        let price = european.price(&market_data, val_date).unwrap();

        // round trip it via JSON
        let serialized = serde_json::to_string_pretty(&market_data).unwrap();
        print!("serialized: {}\n", serialized);
        let deserialized: MarketData = serde_json::from_str(&serialized).unwrap();

        // check that we still get the same price
        let serde_price = european.price(&deserialized, val_date).unwrap();
        assert_approx(serde_price, price, 1e-12);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(
            approx_eq(value, expected, tolerance),
            "value={} expected={}",
            value,
            expected
        );
    }
}
