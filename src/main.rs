mod component;
mod eval;
mod parse;

use yew;

use component::app::App;

fn main() {
    yew::start_app::<App>();
}
