use crate::parse::Exp;
use serde_yaml;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::iter::{FromIterator, IntoIterator};
use std::rc::Rc;
use yew::prelude::*;

use super::super::model::rule::to_string as rule_to_string;
use super::super::model::{Derivation, DerivationItem, DerivationRule};
use super::row::Row;
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
    RuleSelection { item_id: i32 },
    Readonly,
}

pub struct State {
    derivation: Derivation,
    mode: Mode,
    serialized_text: String,
    save_status: Option<Result<String, String>>,
    load_selection: String,
    focused: Option<i32>,
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
        self.derivation.items.splice(index..index, items);
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
    ApplyDerivation(String),
    UpdateSerializedText(String),
    Import,
    Export,
    Save,
    UpdateLoadSelection(String),
    Load,
    RemoveSaved,
    Focus(i32),
    ToggleReadonly,
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
                focused: None,
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
                if self.state.focused == Some(id) {
                    self.state.focused = None
                };
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
            Msg::ApplyDerivation(name) => {
                if let Some(focused_id) = self.state.focused {
                    if let Some(Ok(exp)) = self
                        .state
                        .derivation
                        .item_for_id(focused_id)
                        .map(|i| i.sentence())
                    {
                        if let (Some(derivation), Some(index)) = (
                            self.store.load_by_name(&name),
                            self.state.derivation.index_for_item(focused_id),
                        ) {
                            let map_items = |items: &[DerivationItem],
                                             mapping: &BTreeMap<String, String>,
                                             match_map: (i32, i32)|
                             -> (Vec<DerivationItem>, BTreeMap::<i32, i32>) {
                                let mut next_id = self.state.next_id() - 1;
                                let mut id_map =
                                    BTreeMap::<i32, i32>::from_iter([match_map].iter().cloned());
                                let res: Vec<DerivationItem> = items
                                    .iter()
                                    .map(|item| {
                                        next_id += 1;
                                        id_map.insert(item.id, next_id);
                                        let rule: Option<DerivationRule> =
                                            if let Some(rule) = &item.rule {
                                                Some(rule.id_replaced(&id_map))
                                            } else {
                                                None
                                            };

                                        match item.sentence() {
                                            Ok(sentence) => {
                                                let replaced =
                                                    mapping.iter().fold(sentence, |acc, (k, v)| {
                                                        acc.replaced(
                                                            &Exp::Atom(k.clone(), vec![]),
                                                            &Exp::Atom(v.clone(), vec![]),
                                                        )
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
                                (res, id_map)
                            };
                            let matches = |item: &DerivationItem| {
                                if let Ok(e) = item.sentence() {
                                    e.form_eq(&exp)
                                } else {
                                    None
                                }
                            };
                            if let Some(first) = derivation.items.first() {
                                if let Some(mapping) = matches(first) {
                                    let (items, _) = map_items(
                                        &derivation.items[1..derivation.items.len()],
                                        &mapping,
                                        (first.id, focused_id),
                                    );
                                    self.state.extend_items(index + 1, items);
                                    return true;
                                };
                            };
                            if let Some(last) = derivation.items.last() {
                                if let Some(mapping) = matches(last) {
                                    let (items, id_map) = map_items(
                                        &derivation.items[0..derivation.items.len() - 1],
                                        &mapping,
                                        (last.id, focused_id),
                                    );
                                    self.state.extend_items(index, items);
                                    self.state.update_item_rule(
                                        focused_id,
                                        &last.rule.as_ref().map(|r| r.id_replaced(&id_map)),
                                    );
                                    return true;
                                }
                            }
                            false
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
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
            Msg::Focus(item_id) => {
                self.state.focused = Some(item_id);
                true
            }
            Msg::ToggleReadonly => {
                self.state.mode = match self.state.mode {
                    Mode::Readonly => Mode::Normal,
                    _ => Mode::Readonly,
                };
                true
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let is_readonly = matches!(self.state.mode, Mode::Readonly);
        let is_editable = matches!(self.state.mode, Mode::Normal);
        let format_id = Rc::new(BTreeMap::from_iter(
            self.state
                .derivation
                .items
                .iter()
                .enumerate()
                .map(|(idx, item)| (item.id, (idx + 1).to_string())),
        ));
        let children = self.state
                .derivation
                .items
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    let id = e.id;
                    // FIXME: performance - row마다 계산하면서 앞부분 계산 계속 중복됨. 캐싱 적용 방안 같은 것 찾기
                    let premise_ids: BTreeSet<i32> = self
                        .state
                        .derivation
                        .deps_for_item(e.id)
                        .unwrap_or_else(|| HashSet::new())
                        .iter()
                        .map(|e| e.id)
                        .collect();
                    let handle_click_num = {
                        let mode = self.state.mode.clone();
                        self.link.batch_callback(move |_| match mode {
                            Mode::RuleSelection {
                                item_id: editing_item_id,
                            } => Some(Msg::UpdateItem(
                                editing_item_id,
                                ItemMsg::FillRuleDeps(id),
                            )),
                            _ => None,
                        })
                    };
                    html! {
                        <Row
                            item=e.clone()
                            premise_ids=premise_ids
                            index=i
                            format_id=Rc::clone(&format_id)
                            is_rule_valid=self.state.derivation.is_rule_valid(e)
                            is_focused=self.state.focused == Some(id)
                            is_readonly=is_readonly
                            is_editable=is_editable
                            on_click_num=handle_click_num
                            on_mouseover=self.link.callback(move |_| Msg::Focus(id))
                            on_enter=self.link.callback(move |_| Msg::AppendNewItem { after_index: i })
                            on_input_sentence=self.link.callback(move |event: InputData| {
                                Msg::UpdateItem(id, ItemMsg::UpdateSentence(event.value))
                            })
                            on_edit=self.link.callback(move |_| Msg::BeginRuleSelection { for_item_id: id })
                            on_remove=self.link.callback(move |_| Msg::RemoveItem { id })
                        />
                    }
                });
        let saves = self.store.load();
        let save_status = match &self.state.save_status {
            Some(Ok(msg)) => html! { format!("Saved: {}", msg) },
            Some(Err(msg)) => html! { format!("Error: {}", msg) },
            None => html! {},
        };

        let handle_change_serialized_text = self
            .link
            .callback(|event: InputData| Msg::UpdateSerializedText(event.value));

        let handle_change_load_selection =
            self.link.batch_callback(|event: ChangeData| match event {
                ChangeData::Select(elem) => Some(Msg::UpdateLoadSelection(elem.value())),
                _ => None,
            });

        html! {
            <div class="derivation-table--wrapper">
                <div class="derivation-table--section-main">
                    <div>
                        {"Name: "}
                        <input oninput=self.link.callback(|e: InputData| Msg::UpdateName(e.value)) value=self.state.derivation.name.clone() />
                        <button onclick=self.link.callback(|_| Msg::ToggleReadonly)>{"toggle readonly"}</button>
                    </div>
                    <table class="derivation--table--table">
                        <tr>
                            <th>{"전제번호"}</th>
                            <th>{"번호"}</th>
                            <th>{"문장"}</th>
                            <th>{"도출규칙"}</th>
                            { if let Mode::Readonly = self.state.mode { html! {}} else { html!{
                                <th>{""}</th>
                            } }}
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
                        <textarea rows={"30"} cols={"80"} oninput=handle_change_serialized_text>{ self.state.serialized_text.clone() }</textarea>
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
                <div class="derivation-table--section-menu">
                    { self.state.focused.map_or(html!{}, |item_id| {
                        if let Some(Ok(exp)) = self.state.derivation.item_for_id(item_id).map(|i| i.sentence()) {
                            html! {
                                <div>
                                    <h4>{"Apply available saved derivation:"}</h4>
                                    <ul>
                                        { for self.available_derivations_for(&item_id).into_iter().map(|(typ, derivation)| {
                                            let name = derivation.name.clone();
                                            let handle_click_apply_derivation = self.link.callback(move |_| Msg::ApplyDerivation(name.to_owned()));
                                            html_nested! {
                                                <li>
                                                    <button onclick=handle_click_apply_derivation>{ derivation.name.clone() }</button>
                                                    { typ.describe_opposite(&derivation, &exp) }
                                                </li>
                                            }
                                        }) }
                                    </ul>
                                </div>
                            }
                        } else { html! {} }
                        }) }
                    <hr />
                </div>
            </div>
        }
    }
}

impl DerivationTable {
    fn available_derivations_for(&self, item_id: &i32) -> Vec<(ApplyDerivationType, Derivation)> {
        if let Some(Ok(exp)) = self
            .state
            .derivation
            .item_for_id(*item_id)
            .map(|i| i.sentence())
        {
            self.store
                .load_by_first_exp(&exp)
                .into_iter()
                .map(|d| (ApplyDerivationType::FromFirst, d))
                .chain(
                    self.store
                        .load_by_last_exp(&exp)
                        .into_iter()
                        .map(|d| (ApplyDerivationType::ToLast, d)),
                )
                .collect()
        } else {
            vec![]
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
            Mode::Readonly => html! { "Mode: Readonly" },
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
                    (component
                        .state
                        .derivation
                        .index_for_item(*i)
                        .map_or(0, |n| n + 1))
                    .to_string()
                } else {
                    "_".to_owned()
                }
            };
            let rule_view = match &item.rule {
                None => html! {},
                Some(rule) => html! {
                    rule_to_string(rule, rule_id_to_num, true)
                },
            };

            html! {
                <>
                    { format!("Mode: Editing derivation rule for {}", component.state.derivation.index_for_item(item_id).map_or(0, |n| n + 1)) }
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

enum ApplyDerivationType {
    FromFirst,
    ToLast,
}

impl ApplyDerivationType {
    pub fn matchsite(&self, derivation: &Derivation) -> Option<Exp> {
        let item = match self {
            Self::FromFirst => derivation.items.first(),
            Self::ToLast => derivation.items.last(),
        };
        if let Some(Ok(exp)) = item.map(|i| i.sentence()) {
            Some(exp)
        } else {
            None
        }
    }

    pub fn opposite(&self, derivation: &Derivation) -> Option<Exp> {
        let item = match self {
            Self::FromFirst => derivation.items.last(),
            Self::ToLast => derivation.items.first(),
        };
        if let Some(Ok(exp)) = item.map(|i| i.sentence()) {
            Some(exp)
        } else {
            None
        }
    }

    pub fn to_opposite(&self, derivation: &Derivation, exp: &Exp) -> Option<Exp> {
        if let (Some(matchsite), Some(opposite)) =
            (self.matchsite(derivation), self.opposite(derivation))
        {
            if let Some(mapping) = matchsite.form_eq(exp) {
                Some(mapping.into_iter().fold(opposite, |acc, (k, v)| {
                    acc.replaced(&Exp::Atom(k, vec![]), &Exp::Atom(v, vec![]))
                }))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn describe_opposite(&self, derivation: &Derivation, exp: &Exp) -> String {
        if let Some(opposite) = self.to_opposite(derivation, exp) {
            let opposite = exp_to_string(&opposite);
            match self {
                Self::FromFirst => format!("=> \"{}\"", opposite),
                Self::ToLast => format!("\"{}\" =>", opposite),
            }
        } else {
            "".to_owned()
        }
    }
}
