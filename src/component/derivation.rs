use crate::exp;
use std::cmp::max;
use std::fmt::{self, Display, Formatter};
use yew::prelude::*;

#[derive(Debug)]
pub enum DerivationRule {
    Premise,
    AndIntro(Option<i32>, Option<i32>),
    AndExclude(Option<i32>),
    OrIntro(Option<i32>, Option<i32>),
    OrExclude(
        Option<i32>,
        (Option<i32>, Option<i32>),
        (Option<i32>, Option<i32>),
    ),
    IfIntro((Option<i32>, Option<i32>)),
    IfExclude(Option<i32>, Option<i32>),
    IffIntro(Option<i32>, Option<i32>),
    IffExclude(Option<i32>),
    NegIntro((Option<i32>, Option<i32>)),
    NegExclude((Option<i32>, Option<i32>)),
    UnivQuntIntro(Option<i32>),
    UnivQuntExclude(Option<i32>),
    ExisQuntIntro(Option<i32>),
    ExisQuntExclude(Option<i32>, (Option<i32>, Option<i32>)),
}

impl Display for DerivationRule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let fmt = |i: &Option<i32>| -> String {
            match i {
                None => "_".to_owned(),
                Some(i) => i.to_string(),
            }
        };
        match self {
            DerivationRule::Premise => write!(f, "P"),
            DerivationRule::AndIntro(k, l) => write!(f, "{}, {} &I", fmt(k), fmt(l)),
            DerivationRule::AndExclude(k) => write!(f, "{} &E", fmt(k)),
            DerivationRule::OrIntro(k, l) => match l {
                None => write!(f, "{} ∨I", fmt(k)),
                Some(l) => write!(f, "{}, {} ∨I", fmt(k), l),
            },
            DerivationRule::OrExclude(k, (l1, m1), (l2, m2)) => {
                write!(
                    f,
                    "{}, {}-{}, {}-{} ∨E",
                    fmt(k),
                    fmt(l1),
                    fmt(m1),
                    fmt(l2),
                    fmt(m2)
                )
            }
            DerivationRule::IfIntro((k, l)) => write!(f, "{}-{} →I", fmt(k), fmt(l)),
            DerivationRule::IfExclude(k, l) => write!(f, "{}, {} →E", fmt(k), fmt(l)),
            DerivationRule::IffIntro(k, l) => write!(f, "{}, {} ↔I", fmt(k), fmt(l)),
            DerivationRule::IffExclude(k) => write!(f, "{} ↔E", fmt(k)),
            DerivationRule::NegIntro((k, l)) => write!(f, "{}-{} -I", fmt(k), fmt(l)),
            DerivationRule::NegExclude((k, l)) => write!(f, "{}-{} -E", fmt(k), fmt(l)),
            DerivationRule::UnivQuntIntro(k) => write!(f, "{} ()I", fmt(k)),
            DerivationRule::UnivQuntExclude(k) => write!(f, "{} ()E", fmt(k)),
            DerivationRule::ExisQuntIntro(k) => write!(f, "{} ∃I", fmt(k)),
            DerivationRule::ExisQuntExclude(k, (l, m)) => {
                write!(f, "{}, {}-{} ∃E", fmt(k), fmt(l), fmt(m))
            }
        }
    }
}

#[derive(Debug)]
struct DerivationItem {
    id: i32,
    sentence_text: String,
    rule: Option<DerivationRule>,
}

impl DerivationItem {
    fn is_valid_sentence(&self) -> bool {
        let parsed = exp(&self.sentence_text);
        match parsed {
            Result::Ok(("", _)) => true,
            Result::Ok((_, _)) => false,
            Result::Err(_) => false,
        }
    }
}

pub struct DerivationTable {
    link: ComponentLink<Self>,
    state: State,
}

pub struct State {
    items: Vec<DerivationItem>,
}

impl State {
    fn item_for_id(&self, id: i32) -> Option<&DerivationItem> {
        self.items.iter().find(|e| e.id == id)
    }

    fn item_for_id_mut(&mut self, id: i32) -> Option<&mut DerivationItem> {
        self.items.iter_mut().find(|e| e.id == id)
    }

    fn index_for_item(&self, item: &DerivationItem) -> usize {
        self.items.iter().position(|r| r.id == item.id).unwrap()
    }

    fn append_new_item(&mut self, index: usize) {
        let item = DerivationItem {
            id: self.items.iter().map(|i| i.id).fold(1, |r, i| max(r, i)) + 1,
            sentence_text: "".to_owned(),
            rule: None,
        };
        self.items.insert(index + 1, item);
    }

    fn update_item_sentence(&mut self, id: i32, val: String) {
        if let Some(item) = self.item_for_id_mut(id) {
            item.sentence_text = val.clone();
        }
    }

    fn get_premises_for_item<'a>(&self, item: &'a DerivationItem) -> Vec<&'a DerivationItem> {
        match item.rule {
            Some(DerivationRule::Premise) => vec![item],
            Some(DerivationRule::AndIntro(k, l)) => vec![],
            Some(DerivationRule::AndExclude(k)) => vec![],
            Some(DerivationRule::OrIntro(k, l)) => vec![],
            Some(DerivationRule::OrExclude(k, (l1, m1), (l2, m2))) => vec![],
            Some(DerivationRule::IfIntro((k, l))) => vec![],
            Some(DerivationRule::IfExclude(k, l)) => vec![],
            Some(DerivationRule::IffIntro(k, l)) => vec![],
            Some(DerivationRule::IffExclude(k)) => vec![],
            Some(DerivationRule::NegIntro((k, l))) => vec![],
            Some(DerivationRule::NegExclude((k, l))) => vec![],
            Some(DerivationRule::UnivQuntIntro(k)) => vec![],
            Some(DerivationRule::UnivQuntExclude(k)) => vec![],
            Some(DerivationRule::ExisQuntIntro(k)) => vec![],
            Some(DerivationRule::ExisQuntExclude(k, (l, m))) => vec![],
            None => vec![],
        }
    }
}

pub enum Msg {
    UpdateItem(i32, ItemMsg),
    AppendNewItem { after_index: usize },
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
                items: vec![DerivationItem {
                    id: 1,
                    sentence_text: "".to_owned(),
                    rule: Some(DerivationRule::Premise),
                }],
            },
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::UpdateItem(id, item_msg) => {
                match item_msg {
                    ItemMsg::UpdateSentence(val) => {
                        self.state.update_item_sentence(id, val);
                        true
                    }
                }
            },
            Msg::AppendNewItem { after_index } => {
                self.state.append_new_item(after_index);
                true
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        html! {
            <table>
                <tr>
                    <th>{"전제번호"}</th>
                    <th>{"번호"}</th>
                    <th>{"문장"}</th>
                    <th>{"도출규칙"}</th>
                    <th>{""}</th>
                </tr>
                { for self.state.items.iter().enumerate().map(|(i, e)| self.view_item(e, i)) }
            </table>
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
        let deps = self.state.get_premises_for_item(item);
        let premise_nums: Vec<String> = deps.iter().map(|&e| (self.state.index_for_item(e) + 1).to_string()).collect();

        let fmt_id = |id: i32| (id as u8 + 96) as char;

        html! {
            <tr key={item.id}>
                <td>{ premise_nums.join(",") }</td>
                <td>{ format!("{} ({})", index, fmt_id(item.id)) }</td>
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
                    Some(rule) => rule.to_string(),
                    None => "".to_string(),
                } }</td>
                <td></td>
            </tr>
        }
    }
}
