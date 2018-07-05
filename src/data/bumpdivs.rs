use std::rc::Rc;
use data::divstream::DividendStream;
use data::bump::Bumper;

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

impl Bumper<Rc<DividendStream>> for BumpDivs {

    fn apply(&self, divs: Rc<DividendStream>) -> Rc<DividendStream> {
        match self {
            &BumpDivs::BumpAllRelative { size }
                => Rc::new(DividendStream::new_bump_all(&*divs, size)),
        }
    }
}
