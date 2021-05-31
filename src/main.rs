mod component;
mod eval;
mod parse;
mod service;

extern crate console_error_panic_hook;

use std::panic;
use yew;

use component::app::App;

fn main() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    yew::start_app::<App>();
}
