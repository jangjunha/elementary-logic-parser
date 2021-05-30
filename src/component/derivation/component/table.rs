use crate::parse::Exp;
use serde_yaml;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use yew::prelude::*;

use super::super::model::rule::to_string as rule_to_string;
use super::super::model::{Derivation, DerivationItem, DerivationRule};
use crate::parse::exp as parse_exp;
use crate::parse::model::to_string as exp_to_string;
use crate::service::derivation_store::DerivationStoreService;

pub struct DerivationTable {
    link: ComponentLink<Self>,
    store: DerivationStoreService,
    state: State,
}

#[derive(Debug, Clone)]
enum Mode {
    Normal,
    RuleSelection {
        item_id: i32,
    },
    ApplyDerivation {
        item_id: i32,
        derivation: Derivation,
        mapping: BTreeMap<String, Option<String>>,
    },
}

pub struct State {
    derivation: Derivation,
    mode: Mode,
    serialized_text: String,
    save_status: Option<Result<String, String>>,
    load_selection: String,
}

impl State {
    fn next_id(&self) -> i32 {
        self.derivation
            .items
            .iter()
            .map(|i| i.id)
            .fold(1, |r, i| max(r, i))
            + 1
    }

    fn append_new_item(&mut self, index: usize) {
        let item = DerivationItem {
            id: self.next_id(),
            sentence_text: "".to_owned(),
            rule: None,
        };
        self.derivation.items.insert(index + 1, item);
    }

    // Extend items next given index. Attention to item id (to be not duplicated).
    fn extend_items(&mut self, index: usize, items: Vec<DerivationItem>) {
        self.derivation
            .items
            .splice((index + 1)..(index + 1), items);
    }

    fn remove_item(&mut self, id: i32) {
        self.derivation.items.retain(|i| i.id != id);
    }

    fn update_item_sentence(&mut self, id: i32, val: String) {
        if let Some(item) = self.derivation.item_for_id_mut(id) {
            item.sentence_text = val.clone();
        }
    }

    fn update_item_rule(&mut self, id: i32, rule: &Option<DerivationRule>) {
        if let Some(item) = self.derivation.item_for_id_mut(id) {
            item.rule = rule.clone();
        }
    }
}

pub enum Msg {
    UpdateName(String),
    UpdateItem(i32, ItemMsg),
    AppendNewItem { after_index: usize },
    RemoveItem { id: i32 },
    BeginRuleSelection { for_item_id: i32 },
    ExitRuleSelection,
    BeginApplyDerivation { item_id: i32 },
    UpdateApplyingDerivationMapping(String, String),
    DoneApplyDerivation,
    ExitApplyDerivation,
    UpdateSerializedText(String),
    Import,
    Export,
    Save,
    UpdateLoadSelection(String),
    Load,
    RemoveSaved,
}

pub enum ItemMsg {
    UpdateSentence(String),
    SelectRule(Option<DerivationRule>),
    FillRuleDeps(i32),
}

impl Component for DerivationTable {
    type Properties = ();
    type Message = Msg;

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            store: DerivationStoreService::new(),
            state: State {
                derivation: Derivation {
                    name: "Untitled".to_owned(),
                    items: vec![DerivationItem {
                        id: 1,
                        sentence_text: "".to_owned(),
                        rule: Some(DerivationRule::Premise),
                    }],
                },
                mode: Mode::Normal,
                serialized_text: "".to_owned(),
                save_status: None,
                load_selection: "".to_owned(),
            },
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::UpdateName(val) => {
                self.state.derivation.name = val;
                false
            }
            Msg::UpdateItem(id, item_msg) => match item_msg {
                ItemMsg::UpdateSentence(val) => {
                    self.state.update_item_sentence(id, val);
                    true
                }
                ItemMsg::SelectRule(rule) => {
                    self.state.update_item_rule(id, &rule);
                    if let Some(DerivationRule::Premise) = rule {
                        self.state.mode = Mode::Normal;
                    };
                    true
                }
                ItemMsg::FillRuleDeps(next_id) => {
                    if let Some(Some((next_rule, is_completed))) = self
                        .state
                        .derivation
                        .item_for_id(id)
                        .map(|i| i.rule.as_ref().map(|r| r.filling_next(next_id)))
                    {
                        self.state.update_item_rule(id, &Some(next_rule));
                        if is_completed {
                            self.state.mode = Mode::Normal;
                        }
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
            Msg::RemoveItem { id } => {
                self.state.remove_item(id);
                true
            }
            Msg::BeginRuleSelection {
                for_item_id: item_id,
            } => {
                self.state.mode = Mode::RuleSelection { item_id };
                true
            }
            Msg::ExitRuleSelection => match &self.state.mode {
                Mode::RuleSelection { item_id: _ } => {
                    self.state.mode = Mode::Normal;
                    true
                }
                _ => false,
            },
            Msg::BeginApplyDerivation { item_id } => {
                match self.store.load_by_name(&self.state.load_selection) {
                    Some(derivation) => {
                        let mut mapping = BTreeMap::<String, Option<String>>::new();
                        match derivation.items.first().map(|i| i.sentence()) {
                            Some(Ok(first_exp)) => {
                                fn _atom_exps(
                                    exp: &Exp,
                                    bound_vars: BTreeSet<String>,
                                ) -> BTreeSet<String> {
                                    match exp {
                                        Exp::Atom(_, inds) => {
                                            let inds =
                                                BTreeSet::<String>::from_iter(inds.iter().cloned());
                                            if inds.intersection(&bound_vars).count() > 0 {
                                                // TODO: 처리 방향 고민 필요
                                                BTreeSet::new()
                                            } else {
                                                BTreeSet::from_iter(
                                                    [exp_to_string(exp)].iter().cloned(),
                                                )
                                            }
                                        }
                                        Exp::Cond(lhs, rhs)
                                        | Exp::Iff(lhs, rhs)
                                        | Exp::And(lhs, rhs)
                                        | Exp::Or(lhs, rhs) => _atom_exps(lhs, bound_vars.clone())
                                            .union(&_atom_exps(rhs, bound_vars))
                                            .cloned()
                                            .collect(),
                                        Exp::Neg(lhs) => _atom_exps(lhs, bound_vars),
                                        Exp::UnivGenr(var, inner) | Exp::ExistGenr(var, inner) => {
                                            let mut vars = inner.free_variables();
                                            vars.remove(var);
                                            vars
                                        }
                                        Exp::Falsum => BTreeSet::new(),
                                    }
                                }
                                let atom_exps = |exp| _atom_exps(exp, BTreeSet::new());
                                for exp in atom_exps(&first_exp) {
                                    mapping.insert(exp, None);
                                }
                                self.state.mode = Mode::ApplyDerivation {
                                    item_id,
                                    derivation,
                                    mapping,
                                };
                                true
                            }
                            _ => false,
                        }
                    }
                    None => false,
                }
            }
            Msg::UpdateApplyingDerivationMapping(k, v) => match &mut self.state.mode {
                Mode::ApplyDerivation {
                    item_id: _,
                    derivation: _,
                    mapping,
                } => {
                    let v = if v.is_empty() { None } else { Some(v) };
                    mapping.insert(k, v);
                    true
                }
                _ => false,
            },
            Msg::DoneApplyDerivation => match &self.state.mode {
                Mode::ApplyDerivation {
                    item_id,
                    derivation,
                    mapping,
                } => {
                    let index = self.state.derivation.index_for_item(*item_id);
                    let mut next_id = self.state.next_id() - 1;
                    let mut id_map = BTreeMap::<i32, i32>::new();
                    let items = derivation
                        .items
                        .iter()
                        .map(|item| {
                            next_id += 1;
                            id_map.insert(item.id, next_id);
                            let rule: Option<DerivationRule> = if let Some(rule) = &item.rule {
                                Some(rule.id_replaced(&id_map))
                            } else {
                                None
                            };

                            match item.sentence() {
                                Ok(sentence) => {
                                    let replaced = mapping.iter().fold(sentence, |acc, (k, v)| {
                                        match (parse_exp(k), v.as_ref().map(|v| parse_exp(&v))) {
                                            (Ok((r1, k)), Some(Ok((r2, v))))
                                                if r1.is_empty() && r2.is_empty() =>
                                            {
                                                acc.replaced(&k, &v)
                                            }
                                            _ => acc,
                                        }
                                    });
                                    DerivationItem {
                                        id: next_id,
                                        sentence_text: exp_to_string(&replaced),
                                        rule,
                                    }
                                }
                                Err(_) => DerivationItem {
                                    id: next_id,
                                    sentence_text: "<ERROR>".to_owned(),
                                    rule,
                                },
                            }
                        })
                        .collect();

                    self.state.extend_items(index, items);
                    self.state.mode = Mode::Normal;
                    true
                }
                _ => false,
            },
            Msg::ExitApplyDerivation => match &self.state.mode {
                Mode::ApplyDerivation {
                    item_id: _,
                    derivation: _,
                    mapping: _,
                } => {
                    self.state.mode = Mode::Normal;
                    true
                }
                _ => false,
            },
            Msg::UpdateSerializedText(text) => {
                self.state.serialized_text = text;
                false
            }
            Msg::Import => match serde_yaml::from_str(&self.state.serialized_text) {
                Ok(derivation) => {
                    self.state.mode = Mode::Normal;
                    self.state.derivation = derivation;
                    true
                }
                Err(_) => false,
            },
            Msg::Export => {
                self.state.serialized_text =
                    serde_yaml::to_string(&self.state.derivation).unwrap_or("".to_owned());
                true
            }
            Msg::Save => {
                let name = &self.state.derivation.name;
                let postfix = if let Some(_) = self.store.load_by_name(name) {
                    "overwrited"
                } else {
                    "saved"
                };
                self.store.insert(&self.state.derivation);
                self.state.save_status = Some(Ok(format!("'{}' {}", name, postfix)));
                true
            }
            Msg::UpdateLoadSelection(val) => {
                self.state.load_selection = val;
                false
            }
            Msg::Load => {
                let name = &self.state.load_selection;
                match self.store.load_by_name(name) {
                    Some(derivation) => {
                        self.state.derivation = derivation;
                        self.state.mode = Mode::Normal;
                        true
                    }
                    None => false,
                }
            }
            Msg::RemoveSaved => {
                self.store.remove(&self.state.load_selection);
                self.state.load_selection = "".to_owned();
                true
            }
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
                            if let Some(Some(i)) = l1.map(|e| self.state.derivation.item_for_id(e))
                            {
                                dep.remove(i);
                            }
                            if let Some(Some(i)) = l2.map(|e| self.state.derivation.item_for_id(e))
                            {
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
        let saves = self.store.load();
        let save_status = match &self.state.save_status {
            Some(Ok(msg)) => html! { format!("Saved: {}", msg) },
            Some(Err(msg)) => html! { format!("Error: {}", msg) },
            None => html! {},
        };

        let handle_change_name = self.link.batch_callback(|event: ChangeData| match event {
            ChangeData::Value(val) => Some(Msg::UpdateName(val)),
            _ => None,
        });

        let handle_change_serialized_text =
            self.link.batch_callback(|event: ChangeData| match event {
                ChangeData::Value(val) => Some(Msg::UpdateSerializedText(val)),
                _ => None,
            });

        let handle_change_load_selection =
            self.link.batch_callback(|event: ChangeData| match event {
                ChangeData::Select(elem) => Some(Msg::UpdateLoadSelection(elem.value())),
                _ => None,
            });

        html! {
            <div>
                <div>
                    {"Name: "}
                    <input onchange=handle_change_name value=self.state.derivation.name.clone() />
                </div>
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
                <hr />
                <div>
                    <h3>{"Import/Export"}</h3>
                    <button onclick=self.link.callback(|_| Msg::Export)>{ "export" }</button>
                    <br />
                    <textarea rows={"30"} cols={"80"} onchange=handle_change_serialized_text>{ self.state.serialized_text.clone() }</textarea>
                    <br />
                    <button onclick=self.link.callback(|_| Msg::Import)>{ "import" }</button>
                </div>
                <hr />
                <div>
                    <h3>{"Save/Load"}</h3>
                    <button onclick=self.link.callback(|_| Msg::Save)>{ "save" }</button>
                    { save_status }
                    <br />
                    <select onchange=handle_change_load_selection>
                        <option value="">{"== select =="}</option>
                        { for saves.iter().map(|d| html_nested! {
                            <option value=d.name.clone()>{ d.name.clone() }</option>
                        }) }
                    </select>
                    <button onclick=self.link.callback(|_| Msg::Load)>{ "load" }</button>
                    <button onclick=self.link.callback(|_| Msg::RemoveSaved)>{ "remove" }</button>
                </div>
            </div>
        }
    }
}

impl DerivationTable {
    fn view_item(
        &self,
        item: &DerivationItem,
        index: usize,
        deps: &HashMap<i32, HashSet<&DerivationItem>>,
    ) -> Html {
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
                <td>{ format!("{{{}}}", premise_nums.join(",")) }</td>
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
                    { self.view_item_remove_button(item_id) }
                    { self.view_item_apply_derivation_button(item_id) }
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

    fn view_item_remove_button(&self, item_id: i32) -> Html {
        let disabled = match self.state.mode {
            Mode::Normal => false,
            _ => true,
        };
        html! {
            <button
                onclick=self.link.callback(move |_| Msg::RemoveItem { id: item_id })
                disabled=disabled
            >
                {"remove"}
            </button>
        }
    }

    fn view_item_apply_derivation_button(&self, item_id: i32) -> Html {
        let disabled = match self.state.mode {
            Mode::Normal => false,
            _ => true,
        };
        html! {
            <button
                onclick=self.link.callback(move |_| Msg::BeginApplyDerivation { item_id })
                disabled=disabled
            >
                {"append saved"}
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
            Mode::ApplyDerivation {
                item_id,
                derivation,
                mapping,
            } => self.view_content_apply_derivation(component, item_id, derivation, mapping),
        }
    }

    fn view_content_apply_derivation(
        &self,
        component: &DerivationTable,
        item_id: &i32,
        derivation: &Derivation,
        mapping: &BTreeMap<String, Option<String>>,
    ) -> Html {
        let target_num = component.state.derivation.index_for_item(*item_id) + 1;
        let disabled = !mapping.values().all(|v| v.is_some());
        match (derivation.items.first(), derivation.items.last()) {
            (Some(first_item), Some(last_item)) => {
                html! {
                    <>
                        { format!("Mode: Append derivation \"{}\" => \"{}\" after {}", first_item.sentence_text, last_item.sentence_text, target_num) }
                        <ul>
                            { for mapping.iter().map(|(k, v)| {
                                self.view_content_apply_derivation_mapping_item(component, k.clone(), v.clone())
                            }) }
                        </ul>
                        { "NOTE: After fill all mapping, focus out to enable apply button." }
                        <button onclick=component.link.callback(|_| Msg::DoneApplyDerivation) disabled=disabled>{ "apply" }</button>
                        <button onclick=component.link.callback(|_| Msg::ExitApplyDerivation)>{ "discard" }</button>
                    </>
                }
            }
            _ => html! {},
        }
    }

    fn view_content_apply_derivation_mapping_item(
        &self,
        component: &DerivationTable,
        k: String,
        v: Option<String>,
    ) -> Html {
        let key = k.clone();
        let handle_change_mapping_value =
            component
                .link
                .batch_callback(move |event: ChangeData| match event {
                    ChangeData::Value(val) => {
                        Some(Msg::UpdateApplyingDerivationMapping(key.clone(), val))
                    }
                    _ => None,
                });
        html! {
            <li>
                { format!("{} => ", k.clone()) }
                <input value=v.unwrap_or("".to_owned()) onchange=handle_change_mapping_value />
            </li>
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
                                Some(Msg::UpdateItem(item_id, ItemMsg::SelectRule(Some(rule))))
                            } else {
                                Some(Msg::UpdateItem(item_id, ItemMsg::SelectRule(None)))
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
