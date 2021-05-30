use yew::prelude::*;

use super::super::model::DerivationRule;

pub struct DerivationRuleInput {
    link: ComponentLink<Self>,
    props: Props,
    state: State,
}

#[derive(Properties, Clone, PartialEq)]
pub struct Props {
    pub rule: Option<DerivationRule>,
    pub on_change: Callback<Option<DerivationRule>>,
}

pub struct State {
    is_editing: bool,
}

pub enum Msg {}

impl Component for DerivationRuleInput {
    type Properties = Props;
    type Message = Msg;

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            props: _props,
            state: State { is_editing: false },
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            // Msg::UpdateItem(id, item_msg) => {
            //     match item_msg {
            //         ItemMsg::UpdateSentence(val) => {
            //             self.state.update_item_sentence(id, val);
            //             true
            //         }
            //     }
            // },
            // Msg::AppendNewItem { after_index } => {
            //     self.state.append_new_item(after_index);
            //     true
            // }
        }
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.props != props
    }

    fn view(&self) -> Html {
        let content = match self.state.is_editing {
            false => match &self.props.rule {
                Some(r) => html! {{ r.to_string() }},
                None => html! {},
            },
            true => self.view_editing(),
        };
        html! {
            <div>
                { content }
            </div>
        }
        // match self.state.rule {
        //     None => html! { <div></div> },
        //     Some(DerivationRule::Premise) => html! { <div></div> },
        //     Some(DerivationRule::AndIntro(k, m)) => html! { <div></div> },
        //     Some(DerivationRule::AndExclude(k)) => html! { <div></div> },
        //     Some(DerivationRule::OrIntro(k, m)) => html! { <div></div> },
        //     Some(DerivationRule::OrExclude(k, (m1, l1), (m2, l2))) => html! { <div></div> },
        //     Some(DerivationRule::IfIntro((k, m))) => html! { <div></div> },
        //     Some(DerivationRule::IfExclude(k, m)) => html! { <div></div> },
        //     Some(DerivationRule::IffIntro(k, m)) => html! { <div></div> },
        //     Some(DerivationRule::IffExclude(k)) => html! { <div></div> },
        //     Some(DerivationRule::NegIntro((k, m))) => html! { <div></div> },
        //     Some(DerivationRule::NegExclude((k, m))) => html! { <div></div> },
        //     Some(DerivationRule::UnivQuntIntro(k)) => html! { <div></div> },
        //     Some(DerivationRule::UnivQuntExclude(k)) => html! { <div></div> },
        //     Some(DerivationRule::ExisQuntIntro(k)) => html! { <div></div> },
        //     Some(DerivationRule::ExisQuntExclude(k, (m, l))) => html! { <div></div> },
        // }
    }
}

impl DerivationRuleInput {
    fn view_editing(&self) -> Html {
        html! {
            <select>
                <option></option>
            </select>
        }
    }
}
