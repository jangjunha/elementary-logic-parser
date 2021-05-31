use crate::component::derivation::model::Derivation;
use crate::parse::Exp;
use yew::format::Yaml;
use yew::services::storage::{Area, StorageService};

const KEY_DERIVATIONS: &str = "derivations";

pub struct DerivationStoreService {
    storage: StorageService,
}

impl DerivationStoreService {
    pub fn new() -> Self {
        Self {
            storage: StorageService::new(Area::Local).expect("storage was disabled by the user"),
        }
    }

    pub fn load(&self) -> Vec<Derivation> {
        let Yaml(exists) = self.storage.restore(KEY_DERIVATIONS);
        exists.ok().unwrap_or_else(Vec::<Derivation>::new)
    }

    fn save(&mut self, derivations: &Vec<&Derivation>) {
        self.storage.store(KEY_DERIVATIONS, Yaml(derivations));
    }

    pub fn insert(&mut self, derivation: &Derivation) {
        let derivations = self.load();
        let mut derivations: Vec<&Derivation> = derivations.iter().collect();
        derivations.retain(|d| d.name != derivation.name);
        derivations.push(derivation);
        self.save(&derivations);
    }

    pub fn remove(&mut self, name: &str) {
        let derivations = self.load();
        let mut derivations: Vec<&Derivation> = derivations.iter().collect();
        derivations.retain(|d| d.name != name);
        self.save(&derivations);
    }

    pub fn load_by_name(&self, name: &str) -> Option<Derivation> {
        let mut loaded = self.load();
        match loaded.iter().position(|d| d.name == *name) {
            Some(index) => Some(loaded.remove(index)),
            None => None,
        }
    }

    pub fn load_by_first_exp(&self, exp: &Exp) -> Vec<Derivation> {
        let loaded = self.load();
        loaded
            .into_iter()
            .filter(|d| {
                if let Some(Ok(s)) = d.items.first().map(|item| item.sentence()) {
                    s.form_eq(exp).is_some()
                } else {
                    false
                }
            })
            .collect()
    }

    pub fn load_by_last_exp(&self, exp: &Exp) -> Vec<Derivation> {
        let loaded = self.load();
        loaded
            .into_iter()
            .filter(|d| {
                if let Some(Ok(s)) = d.items.last().map(|item| item.sentence()) {
                    s.form_eq(exp).is_some()
                } else {
                    false
                }
            })
            .collect()
    }
}
