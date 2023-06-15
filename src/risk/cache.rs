use core::qm;
use data::bump::Bump;
use data::curves::RcRateCurve;
use data::forward::Forward;
use data::volsurface::RcVolSurface;
use dates::Date;
use instruments::Instrument;
use instruments::PricingContext;
use risk::dependencies::DependencyCollector;
use risk::marketdata::copy_from_saved;
use risk::marketdata::MarketData;
use risk::marketdata::SavedData;
use risk::Bumpable;
use risk::BumpablePricingContext;
use risk::Saveable;
use std::any::Any;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

/// Use the dependencies information for a product to prefetch the market data
/// needed for calculations. Although the module is called cache, the behaviour
/// is entirely deterministic. We prefetch the data, rather than lazily caching
/// it.
#[derive(Clone)]
pub struct PricingContextPrefetch {
    context: MarketData,
    dependencies: Arc<DependencyCollector>,
    forward_curves: HashMap<String, Arc<Forward>>,
    vol_surfaces: HashMap<String, RcVolSurface>,
}

impl PricingContextPrefetch {
    /// Creates a context wrapper that prefetches forwards and potentially
    /// vol surfaces for efficiency. The MarketData context that is passed in
    /// is immediately cloned, so the PricingContextPrefetch can modify it
    /// for bumping. The dependencies that are passed in are shared and
    /// immutable.
    pub fn new(
        context: &MarketData,
        dependencies: Arc<DependencyCollector>,
    ) -> Result<PricingContextPrefetch, qm::Error> {
        // prefetch the forward curves and vol surfaces
        let mut forward_curves = HashMap::new();
        let mut vol_surfaces = HashMap::new();
        walk_dependencies(
            &context,
            &dependencies,
            &mut forward_curves,
            &mut vol_surfaces,
        )?;

        Ok(PricingContextPrefetch {
            context: context.clone(),
            dependencies: dependencies,
            forward_curves: forward_curves,
            vol_surfaces: vol_surfaces,
        })
    }

    /// Refetch all of the cached data after some change that affects all
    /// dependencies, such as a theta bump
    pub fn refetch_all(&mut self) -> Result<(), qm::Error> {
        self.forward_curves.clear();
        self.vol_surfaces.clear();
        walk_dependencies(
            &self.context,
            &self.dependencies,
            &mut self.forward_curves,
            &mut self.vol_surfaces,
        )
    }

    /// Refetch some of the cached data after a change that affects only the
    /// forward or vol surface on one instrument, such as a delta bump
    pub fn refetch(
        &mut self,
        id: &str,
        bumped_forward: bool,
        bumped_vol: bool,
        saved_forward_curves: Option<&mut HashMap<String, Arc<Forward>>>,
        saved_vol_surfaces: Option<&mut HashMap<String, RcVolSurface>>,
    ) -> Result<bool, qm::Error> {
        // if nothing was bumped, there is nothing to do (this test included
        // here to simplify usage)
        if !bumped_forward && !bumped_vol {
            return Ok(false);
        }

        // whether we are bumping vol or forward, we need the old forward
        let id_string = id.to_string();
        if let Some(fwd) = self.forward_curves.get_mut(&id_string) {
            if let Some(inst) = self.dependencies.instrument_by_id(id) {
                let instrument: &Instrument = &*inst.clone();

                // save the old forward if we are about to bump it
                if bumped_forward {
                    if let Some(s) = saved_forward_curves {
                        s.insert(id.to_string(), fwd.clone());
                    }

                    // Refetch forward: requires instrument and high water mark
                    if let Some(hwm) = self.dependencies.forward_curve_hwm(inst) {
                        *fwd = self.context.forward_curve(instrument, hwm)?;
                    } else {
                        return Err(qm::Error::new("Cannot find forward"));
                    }
                }

                // If we had vol surfaces such as sticky delta surfaces that
                // needed to be updated when the forward was changed, we'd need
                // the following test to be more complicated than just
                // looking at bumped_vol

                // save the old vol surface if we are about to bump it
                if bumped_vol {
                    if let Some(vol) = self.vol_surfaces.get_mut(&id_string) {
                        if let Some(s) = saved_vol_surfaces {
                            s.insert(id_string, vol.clone());
                        }

                        // Refetch vol if required. If vol not found, it may
                        // not be an error if we are responding to a forward
                        // bump, but that code is not implemented yet.
                        if let Some(vol_hwm) = self.dependencies.vol_surface_hwm(inst) {
                            *vol = self
                                .context
                                .vol_surface(instrument, vol_hwm, &|| Ok(fwd.clone()))?;
                        } else {
                            return Err(qm::Error::new("Cannot find vol"));
                        }
                    }
                }
            } else {
                return Err(qm::Error::new("Cannot find instrument"));
            }
        } else {
            return Err(qm::Error::new("Cannot find prefetched forward"));
        }

        Ok(true)
    }
}

fn walk_dependencies(
    context: &MarketData,
    dependencies: &Arc<DependencyCollector>,
    forward_curves: &mut HashMap<String, Arc<Forward>>,
    vol_surfaces: &mut HashMap<String, RcVolSurface>,
) -> Result<(), qm::Error> {
    let forward_dependencies = dependencies.forward_curves();
    let vol_dependencies = dependencies.vol_surfaces();

    for (rc_instrument, high_water_mark) in &*forward_dependencies {
        // fetch the forward curve
        let instrument: &Instrument = rc_instrument.deref();
        let id = instrument.id().to_string();
        let forward = context.forward_curve(instrument, *high_water_mark)?;

        // if there is an associated vol surface, fetch that
        if let Some(vol_hwd) = vol_dependencies.get(rc_instrument) {
            let vol = context.vol_surface(instrument, *vol_hwd, &|| Ok(forward.clone()))?;
            vol_surfaces.insert(id.clone(), vol);
        }

        forward_curves.insert(id, forward);
    }

    Ok(())
}

impl PricingContext for PricingContextPrefetch {
    fn spot_date(&self) -> Date {
        // no point caching this
        self.context.spot_date()
    }

    fn yield_curve(
        &self,
        credit_id: &str,
        high_water_mark: Date,
    ) -> Result<RcRateCurve, qm::Error> {
        // Currently there is no work in fetching a yield curve, so we do
        // not cache this. If yield curves were to be cooked internally, this
        // would change.
        self.context.yield_curve(credit_id, high_water_mark)
    }

    fn spot(&self, id: &str) -> Result<f64, qm::Error> {
        // no point caching this
        self.context.spot(id)
    }

    fn forward_curve(
        &self,
        instrument: &Instrument,
        _high_water_mark: Date,
    ) -> Result<Arc<Forward>, qm::Error> {
        find_cached_data(instrument.id(), &self.forward_curves, "Forward")
    }

    /// Gets a Vol Surface, given any instrument, for example an equity.  Also
    /// specify a high water mark, beyond which we never directly ask for
    /// vols.
    fn vol_surface(
        &self,
        instrument: &Instrument,
        _high_water_mark: Date,
        _forward_fn: &Fn() -> Result<Arc<Forward>, qm::Error>,
    ) -> Result<RcVolSurface, qm::Error> {
        find_cached_data(instrument.id(), &self.vol_surfaces, "Vol Surface")
    }

    fn correlation(&self, first: &Instrument, second: &Instrument) -> Result<f64, qm::Error> {
        self.context.correlation(first, second)
    }
}

/// Look for market-data-derived objects in the cache. If they are not there,
/// it means that the instrument lied about its dependencies, so return an
/// error. If the high water mark mismatches, this will result in errors later
/// on when the data is used.
fn find_cached_data<T: Clone>(
    id: &str,
    collection: &HashMap<String, T>,
    item: &str,
) -> Result<T, qm::Error> {
    match collection.get(id) {
        None => Err(qm::Error::new(&format!(
            "{} not found (incorrect dependencies?): '{}'",
            item, id
        ))),
        Some(x) => Ok(x.clone()),
    }
}

impl Bumpable for PricingContextPrefetch {
    fn bump(&mut self, bump: &Bump, any_saved: Option<&mut Saveable>) -> Result<bool, qm::Error> {
        //    saved_data: SavedData,
        //    forward_curves: HashMap<String, Rc<Forward>>,
        //    vol_surfaces: HashMap<String, RcVolSurface>

        // we have to unpack the option<saveable> into options on all its
        // components all at the same time, to avoid problems with borrowing.
        let saved = to_saved(any_saved)?;
        let (saved_data, saved_forward_curves, saved_vol_surfaces): (
            Option<&mut Saveable>,
            Option<&mut HashMap<String, Arc<Forward>>>,
            Option<&mut HashMap<String, RcVolSurface>>,
        ) = if let Some(s) = saved {
            (
                Some(&mut s.saved_data),
                Some(&mut s.forward_curves),
                Some(&mut s.vol_surfaces),
            )
        } else {
            (None, None, None)
        };

        // Delegate to the underlying market data to do the actual bumping
        // except for SpotDate, which raises an error. For SpotDate, just
        // set the bumped flag according to whether anything needs to be done.
        let bumped = if let &Bump::SpotDate(ref bump) = bump {
            bump.spot_date() != self.spot_date()
        } else {
            self.context.bump(bump, saved_data)?
        };

        // we may need to refetch some of the prefetched data
        match bump {
            &Bump::Spot(ref id, _) => {
                self.refetch(&id, bumped, false, saved_forward_curves, saved_vol_surfaces)
            }
            &Bump::Divs(ref id, _) => {
                self.refetch(&id, bumped, false, saved_forward_curves, saved_vol_surfaces)
            }
            &Bump::Vol(ref id, _) => {
                self.refetch(&id, false, bumped, saved_forward_curves, saved_vol_surfaces)
            }
            &Bump::Borrow(ref id, _) => {
                self.refetch(&id, bumped, false, saved_forward_curves, saved_vol_surfaces)
            }
            &Bump::Yield(ref credit_id, _) => {
                // we have to copy these ids to avoid a tangle with borrowing
                let v = self
                    .dependencies
                    .forward_id_by_credit_id(&credit_id)
                    .to_vec();

                // We also have to unpack then repack saved_forward curves
                // and saved_vol_surfaces to clarify borrowing. Pretty ugly.
                // (is this something the Rust compiler could be cleverer about?)
                let mut done = false;
                if let Some(sfc) = saved_forward_curves {
                    if let Some(svs) = saved_vol_surfaces {
                        done = true;
                        for id in v.iter() {
                            self.refetch(&id, bumped, false, Some(sfc), Some(svs))?;
                        }
                    }
                }

                if !done {
                    for id in v.iter() {
                        self.refetch(&id, bumped, false, None, None)?;
                    }
                }

                Ok(bumped)
            }
            &Bump::SpotDate(ref bump) => {
                if bumped {
                    self.context.bump_spot_date(bump, &self.dependencies)?;
                    self.refetch_all()?;
                }
                Ok(bumped)
            }
        }
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        Ok(&*self.dependencies)
    }

    fn context(&self) -> &PricingContext {
        self.as_pricing_context()
    }

    fn new_saveable(&self) -> Box<Saveable> {
        Box::new(SavedPrefetch::new())
    }

    fn restore(&mut self, any_saved: &Saveable) -> Result<(), qm::Error> {
        if let Some(saved) = any_saved.as_any().downcast_ref::<SavedPrefetch>() {
            // first restore the underlying market data
            self.context.restore(&saved.saved_data)?;

            // now restore any cached items
            copy_from_saved(&mut self.forward_curves, &saved.forward_curves);
            copy_from_saved(&mut self.vol_surfaces, &saved.vol_surfaces);
            Ok(())
        } else {
            Err(qm::Error::new("Mismatching save space for restore"))
        }
    }
}

impl BumpablePricingContext for PricingContextPrefetch {
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
        &self.context
    }
}

fn to_saved(opt_any_saved: Option<&mut Saveable>) -> Result<Option<&mut SavedPrefetch>, qm::Error> {
    if let Some(any_saved) = opt_any_saved {
        if let Some(saved) = any_saved.as_mut_any().downcast_mut::<SavedPrefetch>() {
            Ok(Some(saved))
        } else {
            Err(qm::Error::new("Mismatching save space for bumped prefetch"))
        }
    } else {
        Ok(None)
    }
}

/// Data structure for saving the prefetched content before a bump, so it
/// can be restored later on.
pub struct SavedPrefetch {
    saved_data: SavedData,
    forward_curves: HashMap<String, Arc<Forward>>,
    vol_surfaces: HashMap<String, RcVolSurface>,
}

impl SavedPrefetch {
    /// Creates an empty market data object, which can be used for saving state
    /// so it can be restored after a bump
    pub fn new() -> SavedPrefetch {
        SavedPrefetch {
            saved_data: SavedData::new(),
            forward_curves: HashMap::new(),
            vol_surfaces: HashMap::new(),
        }
    }
}

impl Saveable for SavedPrefetch {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_mut_any(&mut self) -> &mut Any {
        self
    }

    fn clear(&mut self) {
        self.saved_data.clear();
        self.forward_curves.clear();
        self.vol_surfaces.clear();
    }
}

// These tests are almost literally copied from marketdata.rs. They should
// behave exactly the same way, though potentially rather quicker, as the
// only effect of prefetching should be to speed things up.
#[cfg(test)]
pub mod tests {
    use super::*;
    use core::factories::Qrc;
    use data::bumpdivs::BumpDivs;
    use data::bumpspot::BumpSpot;
    use data::bumpvol::BumpVol;
    use data::bumpyield::BumpYield;
    use dates::datetime::DateTime;
    use dates::datetime::TimeOfDay;
    use instruments::DependencyContext;
    use instruments::Priceable;
    use instruments::RcInstrument;
    use math::numerics::approx_eq;
    use risk::marketdata::tests::sample_european;
    use risk::marketdata::tests::sample_market_data;
    use std::sync::Arc;

    pub fn create_dependencies(
        instrument: &RcInstrument,
        spot_date: Date,
    ) -> Arc<DependencyCollector> {
        let mut collector = DependencyCollector::new(spot_date);
        collector.spot(instrument);
        Arc::new(collector)
    }

    #[test]
    fn european_bumped_price_with_prefetch() {
        let market_data = sample_market_data();
        let european = sample_european();
        let val_date = DateTime::new(Date::from_ymd(2017, 01, 02), TimeOfDay::Open);
        let unbumped_price = european.price(&market_data, val_date).unwrap();

        // Create a prefetch object, which internally clones the market data
        // so we can modify it and also create an
        // empty saved data to save state so we can restore it
        let spot_date = Date::from_ymd(2017, 01, 02);
        let instrument = RcInstrument::new(Qrc::new(european.clone()));
        let dependencies = create_dependencies(&instrument, spot_date);
        let mut mut_data = PricingContextPrefetch::new(&market_data, dependencies).unwrap();
        let mut save = SavedPrefetch::new();

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

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(
            approx_eq(value, expected, tolerance),
            "value={} expected={}",
            value,
            expected
        );
    }
}
