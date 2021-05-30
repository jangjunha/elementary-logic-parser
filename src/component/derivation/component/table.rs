use std::cmp::max;
use std::collections::{HashMap, HashSet};
use yew::prelude::*;

use super::super::model::rule::to_string as rule_to_string;
use super::super::model::{Derivation, DerivationItem, DerivationRule};

pub struct DerivationTable {
    link: ComponentLink<Self>,
    state: State,
}

#[derive(Debug, Clone)]
enum Mode {
    Normal,
    RuleSelection { item_id: i32 },
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

    fn update_item_rule(&mut self, id: i32, rule: Option<DerivationRule>) {
        if let Some(item) = self.derivation.item_for_id_mut(id) {
            item.rule = rule;
        }
    }
}

pub enum Msg {
    UpdateItem(i32, ItemMsg),
    AppendNewItem { after_index: usize },
    BeginRuleSelection { for_item_id: i32 },
    ExitRuleSelection,
}

pub enum ItemMsg {
    UpdateSentence(String),
    UpdateRule(Option<DerivationRule>),
    FillRuleDeps(i32),
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
                ItemMsg::UpdateRule(rule) => {
                    self.state.update_item_rule(id, rule);
                    true
                }
                ItemMsg::FillRuleDeps(next_id) => {
                    if let Some(Some(next_rule)) = self
                        .state
                        .derivation
                        .item_for_id(id)
                        .map(|i| i.rule.as_ref().map(|r| r.filled_next(next_id)))
                    {
                        self.state.update_item_rule(id, Some(next_rule));
                        true
                    } else {
                        false
                    }
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
                    self.state.mode = Mode::RuleSelection { item_id };
                    true
                } else {
                    false
                }
            }
            Msg::ExitRuleSelection => match &self.state.mode {
                Mode::RuleSelection { item_id: _ } => {
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
        let mut deps: HashMap<i32, HashSet<&DerivationItem>> = HashMap::new();
        let children = self
            .state
            .derivation
            .items
            .iter()
            .enumerate()
            .map(|(i, e)| {
                let mut dep = HashSet::<&DerivationItem>::new();
                match &e.rule {
                    Some(rule) => match rule {
                        DerivationRule::Premise => {
                            &dep.insert(e);
                        }
                        DerivationRule::AndIntro(k, l) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::AndExclude(k) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::OrIntro(k, l) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::OrExclude(k, (l1, m1), (l2, m2)) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = m1.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = m2.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l1.map(|e| self.state.derivation.item_for_id(e)) {
                                dep.remove(i);
                            }
                            if let Some(Some(i)) = l2.map(|e| self.state.derivation.item_for_id(e)) {
                                dep.remove(i);
                            }
                        }
                        DerivationRule::IfIntro((k, l)) => {
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = k.map(|e| self.state.derivation.item_for_id(e)) {
                                dep.remove(i);
                            }
                        }
                        DerivationRule::IfExclude(k, l) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::Falsum(k) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::NegIntro((k, l)) | DerivationRule::NegExclude((k, l)) => {
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = k.map(|e| self.state.derivation.item_for_id(e)) {
                                dep.remove(i);
                            }
                        }
                        DerivationRule::IffIntro(k, l) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::IffExclude(k) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::UnivQuntIntro(k) | DerivationRule::UnivQuntExclude(k) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::ExisQuntIntro(k) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                        }
                        DerivationRule::ExisQuntExclude(k, (l, m)) => {
                            if let Some(Some(i)) = k.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = m.map(|e| deps.get(&e)) {
                                dep.extend(i);
                            }
                            if let Some(Some(i)) = l.map(|e| self.state.derivation.item_for_id(e)) {
                                dep.remove(i);
                            }
                        }
                    },
                    None => (),
                };
                deps.insert(e.id, dep);
                self.view_item(e, i, &deps)
            });

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
                    { for children }
                </table>
                <p>{"Press Enter key to insert row below current row."}</p>
                { self.state.mode.view(self) }
            </div>
        }
    }
}

impl DerivationTable {
    fn view_item(&self, item: &DerivationItem, index: usize, deps: &HashMap<i32, HashSet<&DerivationItem>>) -> Html {
        let item_id = item.id;
        let sentence_valid_class = if item.is_valid_sentence() {
            "valid"
        } else {
            "invalid"
        };
        let rule_valid_class = if self.state.derivation.is_rule_valid(item) {
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

        let current_mode = self.state.mode.clone();
        let handle_click_num = self.link.batch_callback(move |_| match current_mode {
            Mode::RuleSelection {
                item_id: editing_item_id,
            } => Some(Msg::UpdateItem(
                editing_item_id,
                ItemMsg::FillRuleDeps(item_id),
            )),
            _ => None,
        });

        let mut premise_nums: Vec<String> = deps
            .get(&item_id)
            .unwrap_or(&HashSet::new())
            .iter()
            .map(|&e| (self.state.derivation.index_for_item(e.id) + 1).to_string())
            .collect();
        premise_nums.sort();

        let rule_id_to_num = |i: &Option<i32>| {
            if let Some(i) = i {
                (self.state.derivation.index_for_item(*i) + 1).to_string()
            } else {
                "_".to_owned()
            }
        };
        let fmt_id = |id: i32| (id as u8 + 96) as char;

        html! {
            <tr key={item.id}>
                <td>{ premise_nums.join(",") }</td>
                <td onclick=handle_click_num>{ format!("{} ({})", index + 1, fmt_id(item.id)) }</td>
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
                <td class=classes!(rule_valid_class)>{ match &item.rule {
                    Some(rule) => rule_to_string(rule, rule_id_to_num),
                    None => "".to_string(),
                } }</td>
                <td>
                    { self.view_item_edit_button(item_id) }
                </td>
            </tr>
        }
    }

    fn view_item_edit_button(&self, item_id: i32) -> Html {
        let disabled = match self.state.mode {
            Mode::Normal => false,
            _ => true,
        };
        html! {
            <button
                onclick=self.link.callback(move |_| Msg::BeginRuleSelection { for_item_id: item_id })
                disabled=disabled
            >
                {"edit rule"}
            </button>
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
            Mode::RuleSelection { item_id } => {
                self.view_content_rule_selection(component, *item_id)
            }
        }
    }

    fn view_content_rule_selection(&self, component: &DerivationTable, item_id: i32) -> Html {
        if let Some(item) = component.state.derivation.item_for_id(item_id) {
            let options = DerivationRule::all_names().iter().map(|&name| {
                let selected = item
                    .rule
                    .as_ref()
                    .map_or_else(|| false, |r| r.name() == name);
                html_nested! {
                    <option value=name key=name selected=selected>{name}</option>
                }
            });
            let handle_change =
                component
                    .link
                    .batch_callback(move |event: ChangeData| match event {
                        ChangeData::Select(elem) => {
                            if let Ok(rule) = DerivationRule::from_name(&elem.value()) {
                                Some(Msg::UpdateItem(item_id, ItemMsg::UpdateRule(Some(rule))))
                            } else {
                                Some(Msg::UpdateItem(item_id, ItemMsg::UpdateRule(None)))
                            }
                        }
                        _ => None,
                    });
            let rule_id_to_num = |i: &Option<i32>| {
                if let Some(i) = i {
                    (component.state.derivation.index_for_item(*i) + 1).to_string()
                } else {
                    "_".to_owned()
                }
            };
            let rule_view = match &item.rule {
                None => html! {},
                Some(rule) => html! {
                    rule_to_string(rule, rule_id_to_num)
                },
            };

            html! {
                <>
                    { format!("Mode: Editing derivation rule for {}", component.state.derivation.index_for_item(item_id) + 1) }
                    <button onclick=component.link.callback(|_| Msg::ExitRuleSelection)>{ "exit" }</button>
                    <div>
                        <select onchange=handle_change>
                            <option value={""} selected=item.rule == None>{"== select rule =="}</option>
                            { for options }
                        </select>
                        { " " }
                        { rule_view }
                    </div>
                </>
            }
        } else {
            html! {{ "Unexpected error: Cannot find item for editing" }}
        }
    }
}
