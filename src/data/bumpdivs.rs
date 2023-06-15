use data::bump::Bumper;
use data::divstream::DividendStream;
use data::divstream::RcDividendStream;
use std::sync::Arc;

/// Bump that defines all the supported bumps and risk transformations of a
/// vol surface.
pub enum BumpDivs {
    BumpAllRelative { size: f64 },
}

impl BumpDivs {
    pub fn new_all_relative(size: f64) -> BumpDivs {
        BumpDivs::BumpAllRelative { size: size }
    }
}

impl Bumper<RcDividendStream> for BumpDivs {
    fn apply(&self, divs: RcDividendStream) -> RcDividendStream {
        match self {
            &BumpDivs::BumpAllRelative { size } => {
                RcDividendStream::new(Arc::new(DividendStream::new_bump_all(&*divs, size)))
            }
        }
    }
}
