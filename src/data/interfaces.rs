use dates::Date;

/// Low-level interfaces to market data

/// Discount curve. Returns the log of the discount rather than the discount
/// itself, as this is very often what we want. It is easy to calculate the
/// discount yourself if you need it, using exp.
pub trait Discount {

    /// Returns the log of the discount factor from the given date to the
    /// base date. If you want the discount factor to any other date, you
    /// must ask for two discount factors and do a division (or subtraction
    /// in log space).
    fn log_df(&self, date: Date) -> Result<f64, qm::Error>;

    /// Returns the base date for discount factor calculations
    fn base(&self) -> Date;

    /// Convenience method that returns the discount factor, rather than
    /// its log
    fn df(&self, date: Date) -> Result<f64, qm::Error> {
        let log_df = self.log_df(date)?;
        log_df.exp()
    }
}

/// Forward curve
pub trait Forward {

    /// Returns the forward on the given date. For example, this may be
    /// the equity forward. In almost all cases, forwards can be considered
    /// piecewise constant over a day. The exception is where there is a
    /// quanto correction. In that case, we use an alternative interface.
    fn forward(&self, date: Date) -> Result<f64, qm::Error>;
}

/// A volatility surface is externally represented as a variance, which means
/// we do not have to deal in time outside the vol surface.
pub trait Variance {

    /// Returns the variances on the given date and time fraction, for a
    /// range of strikes. The day fraction runs from zero at the very start of
    /// the day to one at the end of the day. Known times such as 'open' and
    /// 'close' are somewhere in this range, where the exact position depends
    /// on the asset.
    fn variances(&self,
        date: Date, 
        day_fraction: f64,
        strikes: &[f64], 
        variances: &mut[f64]) -> Result<(), qm::Error>;
}

