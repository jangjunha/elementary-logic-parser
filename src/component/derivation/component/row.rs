use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use boolinator::Boolinator;
use yew::prelude::*;

use super::super::model::rule::to_string as rule_to_string;
use crate::component::derivation::model::DerivationItem;

#[derive(Properties, Clone, PartialEq)]
pub struct RowProps {
    pub item: DerivationItem,
    pub premise_ids: BTreeSet<i32>,
    pub format_id: Rc<BTreeMap<i32, String>>,
    pub is_rule_valid: bool,
    #[prop_or(false)]
    pub is_focused: bool,
    #[prop_or(false)]
    pub is_readonly: bool,
    #[prop_or(true)]
    pub is_editable: bool,
    #[prop_or_default]
    pub on_click_num: Callback<MouseEvent>,
    #[prop_or_default]
    pub on_mouseover: Callback<MouseEvent>,
    #[prop_or_default]
    pub on_enter: Callback<()>,
    #[prop_or_default]
    pub on_input_sentence: Callback<InputData>,
    #[prop_or_default]
    pub on_edit: Callback<()>,
    #[prop_or_default]
    pub on_remove: Callback<()>,
}

pub struct Row {
    link: ComponentLink<Self>,
    props: RowProps,
}

impl Component for Row {
    type Properties = RowProps;
    type Message = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            props: _props,
        }
    }

    fn update(&mut self, _: Self::Message) -> ShouldRender {
        false
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        let res = self.props != _props;
        self.props = _props;
        res
    }

    fn view(&self) -> Html {
        let item = &self.props.item;
        let sentence_valid_class = if item.is_valid_sentence() {
            "valid"
        } else {
            "invalid"
        };
        let rule_valid_class = if self.props.is_rule_valid {
            "valid"
        } else {
            "invalid"
        };
        let format_id = {
            let format_id = Rc::clone(&self.props.format_id);
            move |i: &Option<i32>| {
                if let Some(i) = i {
                    format_id.get(i).unwrap_or(&format!("<{}>", i)).clone()
                } else {
                    "_".to_owned()
                }
            }
        };
        let premise_nums = self
            .props
            .premise_ids
            .iter()
            .map(|i| format_id(&Some(*i)))
            .collect::<Vec<String>>()
            .join(",");

        let handle_keypress = {
            let on_enter = self.props.on_enter.clone();
            self.link.callback(move |event: KeyboardEvent| {
                if event.key() == "Enter" {
                    on_enter.emit(());
                }
            })
        };
        let handle_mouseover = self.props.on_mouseover.clone();
        let handle_click_num = self.props.on_click_num.clone();
        let handle_input_sentence = self.props.on_input_sentence.clone();

        html! {
            <tr key={item.id} onmouseover=handle_mouseover class=classes!((self.props.is_focused).as_some("focused"))>
                <td class="premise-nums">{ format!("{{{}}}", premise_nums) }</td>
                <td onclick=handle_click_num class="num">{ format!("({})", format_id(&Some(item.id))) }</td>
                <td class=classes!(sentence_valid_class, "derivation-table--td-sentence")>
                    { if self.props.is_readonly { html! { item.sentence_text.clone()
                    }} else { html! {
                    <input
                        type="text"
                        value=item.sentence_text.clone()
                        oninput=handle_input_sentence
                        onkeypress=handle_keypress
                    />
                    }}}
                </td>
                <td class=classes!(rule_valid_class, "rule")>{ match &item.rule {
                    Some(rule) => rule_to_string(rule, format_id, false),
                    None => "".to_string(),
                } }</td>
                { if self.props.is_readonly { html! {}} else { html!{
                    <td>
                    { self.view_edit_button() }
                    { self.view_remove_button() }
                </td>
                } }}
            </tr>
        }
    }
}

impl Row {
    fn view_edit_button(&self) -> Html {
        html! {
            <button
                onclick=self.props.on_edit.reform(|_| ())
                disabled=!self.props.is_editable
            >
                {"edit rule"}
            </button>
        }
    }

    fn view_remove_button(&self) -> Html {
        html! {
            <button
                onclick=self.props.on_remove.reform(|_| ())
                disabled=!self.props.is_editable
            >
                {"remove"}
            </button>
        }
    }
}
