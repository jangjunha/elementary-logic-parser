use yew::prelude::*;

use super::derivation::DerivationTable;

pub struct App {
    link: ComponentLink<Self>,
}

impl Component for App {
    type Properties = ();
    type Message = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self { link }
    }

    fn update(&mut self, _msg: Self::Message) -> ShouldRender {
        false
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <p>{"Elementary Logic Utility"}</p>
                <DerivationTable />
            </div>
        }
    }
}
