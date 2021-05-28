mod component;
mod eval;
mod parse;

use std::io::{self};

use yew;

use component::app::App;
use parse::exp;

fn main() {
    yew::start_app::<App>();
}
