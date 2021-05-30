mod component;
mod eval;
mod parse;
mod service;

use yew;

use component::app::App;

fn main() {
    yew::start_app::<App>();
}
