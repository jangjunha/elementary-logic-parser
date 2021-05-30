use std::cmp::max;
use yew::prelude::*;

use super::super::model::rule::to_string as rule_to_string;
use super::super::model::{Derivation, DerivationItem, DerivationRule};

pub struct DerivationTable {
    link: ComponentLink<Self>,
    state: State,
}

enum Mode {
    Normal,
    RuleSelection {
        item_id: i32,
        rule: Option<DerivationRule>,
    },
}

pub struct State {
    derivation: Derivation,
    mode: Mode,
}

impl State {
    fn append_new_item(&mut self, index: usize) {
        let item = DerivationItem {
            id: self
                .derivation
                .items
                .iter()
                .map(|i| i.id)
                .fold(1, |r, i| max(r, i))
                + 1,
            sentence_text: "".to_owned(),
            rule: None,
        };
        self.derivation.items.insert(index + 1, item);
    }

    fn update_item_sentence(&mut self, id: i32, val: String) {
        if let Some(item) = self.derivation.item_for_id_mut(id) {
            item.sentence_text = val.clone();
        }
    }
}

pub enum Msg {
    UpdateItem(i32, ItemMsg),
    AppendNewItem { after_index: usize },
    BeginRuleSelection { for_item_id: i32 },
    DiscardRuleSelection,
}

pub enum ItemMsg {
    UpdateSentence(String),
}

impl Component for DerivationTable {
    type Properties = ();
    type Message = Msg;

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            state: State {
                derivation: Derivation {
                    items: vec![DerivationItem {
                        id: 1,
                        sentence_text: "".to_owned(),
                        rule: Some(DerivationRule::Premise),
                    }],
                },
                mode: Mode::Normal,
            },
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::UpdateItem(id, item_msg) => match item_msg {
                ItemMsg::UpdateSentence(val) => {
                    self.state.update_item_sentence(id, val);
                    true
                }
            },
            Msg::AppendNewItem { after_index } => {
                self.state.append_new_item(after_index);
                true
            }
            Msg::BeginRuleSelection {
                for_item_id: item_id,
            } => {
                if let Some(item) = self.state.derivation.item_for_id(item_id) {
                    self.state.mode = Mode::RuleSelection {
                        item_id,
                        rule: item.rule.clone(),
                    };
                    true
                } else {
                    false
                }
            }
            Msg::DiscardRuleSelection => match &self.state.mode {
                Mode::RuleSelection {
                    item_id: _,
                    rule: _,
                } => {
                    self.state.mode = Mode::Normal;
                    true
                }
                _ => false,
            },
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <table>
                    <tr>
                        <th>{"전제번호"}</th>
                        <th>{"번호"}</th>
                        <th>{"문장"}</th>
                        <th>{"도출규칙"}</th>
                        <th>{""}</th>
                    </tr>
                    { for self.state.derivation.items.iter().enumerate().map(|(i, e)| self.view_item(e, i)) }
                </table>
                <p>{"Press Enter key to insert row below current row."}</p>
                { self.state.mode.view(self) }
            </div>
        }
    }
}

impl DerivationTable {
    fn view_item(&self, item: &DerivationItem, index: usize) -> Html {
        let item_id = item.id;
        let sentence_valid_class = if item.is_valid_sentence() {
            "valid"
        } else {
            "invalid"
        };
        let handle_keypress = self.link.batch_callback(move |event: KeyboardEvent| {
            if event.key() == "Enter" {
                Some(Msg::AppendNewItem { after_index: index })
            } else {
                None
            }
        });
        let deps = self.state.derivation.get_premises_for_item(item);
        let premise_nums: Vec<String> = deps
            .iter()
            .map(|&e| (self.state.derivation.index_for_item(e.id) + 1).to_string())
            .collect();

        let fmt_id = |id: i32| (id as u8 + 96) as char;

        html! {
            <tr key={item.id}>
                <td>{ premise_nums.join(",") }</td>
                <td>{ format!("{} ({})", index + 1, fmt_id(item.id)) }</td>
                <td class=classes!(sentence_valid_class)>
                    <input
                        type="text"
                        value=item.sentence_text.clone()
                        oninput=self.link.callback(move |e: InputData| {
                            Msg::UpdateItem(item_id, ItemMsg::UpdateSentence(e.value))
                        })
                        onkeypress=handle_keypress
                    />
                </td>
                <td>{ match &item.rule {
                    Some(rule) => rule_to_string(rule),
                    None => "".to_string(),
                } }</td>
                <td>
                    { self.view_item_edit_button(item_id) }
                </td>
            </tr>
        }
    }

    fn view_item_edit_button(&self, item_id: i32) -> Html {
        match self.state.mode {
            Mode::Normal => html! {
                <button onclick=self.link.callback(move |_| Msg::BeginRuleSelection { for_item_id: item_id })>{"edit rule"}</button>
            },
            _ => html! {},
        }
    }
}

impl Mode {
    fn view(&self, component: &DerivationTable) -> Html {
        html! {
            <div>
                { self.view_content(component) }
            </div>
        }
    }

    fn view_content(&self, component: &DerivationTable) -> Html {
        match self {
            Mode::Normal => html! { "Mode: Normal" },
            Mode::RuleSelection { item_id, rule } => {
                let options = DerivationRule::all_names().iter().map(|&name| {
                    let selected = rule.as_ref().map_or_else(|| false, |r| r.name() == name);
                    html_nested! {
                        <option value=name key=name selected=selected>{name}</option>
                    }
                });
                let handle_change = component.link.batch_callback(|event: ChangeData| {
                    match event {
                        ChangeData::Select(elem) => {
                            let aa = elem.value();
                            Some(Msg::DiscardRuleSelection)
                        },
                        _ => None
                    }
                });
                html! {
                    <>
                        { format!("Mode: Editing derivation rule for {}", component.state.derivation.index_for_item(*item_id) + 1) }
                        <button onclick=component.link.callback(|_| Msg::DiscardRuleSelection)>{ "discard" }</button>
                        <div>
                            <select onchange=handle_change>
                                { for options }
                            </select>
                        </div>
                    </>
                }
            }
        }
    }
}
