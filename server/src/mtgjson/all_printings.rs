use super::mana::Color;
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};
use std::collections::HashMap;

use super::enums::{SubTypes, SuperTypes, Types};

#[derive(Serialize, Deserialize, PartialEq)]
struct Foreign {
    language: Language,
    name: String,
    text: Option<String>,

    #[serde(rename = "type")]
    type_text: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Clone, Copy)]
enum Language {
    #[serde(rename = "Ancient Greek")]
    AncientGreek,
    Arabic,
    #[serde(rename = "Chinese Simplified")]
    ChineseSimplified,
    #[serde(rename = "Chinese Traditional")]
    ChineseTraditional,
    French,
    German,
    Hebrew,
    Italian,
    Japanese,
    Korean,
    Latin,
    Phyrexian,
    #[serde(rename = "Portuguese (Brazil)")]
    Portuguese,
    Russian,
    Sanskrit,
    Spanish,
}

fn serialize_language<S>(
    target: &HashMap<Language, Foreign>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    use serde::ser::SerializeSeq;
    let mut sm = serializer.serialize_seq(Some(target.len()))?;
    for (k, v) in target.iter() {
        sm.serialize_element(v)?;
    }
    sm.end()
}

fn deserialize_language<'de, D>(deserializer: D) -> Result<HashMap<Language, Foreign>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: Vec<Foreign> = Deserialize::deserialize(deserializer)?;
    let mut res: HashMap<Language, Foreign> = HashMap::new();
    for item in s {
        res.insert(item.language, item);
    }
    Ok(res)
}

#[serde(rename_all = "camelCase")]
#[derive(Serialize, Deserialize, PartialEq)]
struct CardData {
    colors: Vec<Color>,
    converted_mana_cost: f32,
    #[serde(
        serialize_with = "serialize_language",
        deserialize_with = "deserialize_language"
    )]
    foreign_data: HashMap<Language, Foreign>,
    legalities: HashMap<String, String>,

    #[serde(default = "default_leadership_skills")]
    leadership_skills: HashMap<String, bool>,
    color_identity: Vec<Color>,
    mana_cost: Option<String>,

    power: Option<String>,
    toughness: Option<String>,

    name: String,
    text: Option<String>,

    supertypes: Vec<SuperTypes>,
    types: Vec<Types>,
    subtypes: Vec<SubTypes>,

    identifiers: HashMap<String, String>,
    uuid: String,
    #[serde(default = "Vec::new")]
    keywords: Vec<String>,
}
fn default_leadership_skills() -> HashMap<String, bool> {
    HashMap::new()
}

#[derive(Serialize, Deserialize)]
struct CardSet {
    cards: Vec<CardData>,
}

#[derive(Serialize, Deserialize)]
struct AllPrintings {
    data: HashMap<String, CardSet>,
}

struct AtomicCard<'a> {
    card_data: &'a CardData,
    expansions: Vec<String>,
}

impl<'a> AtomicCard<'a> {
    fn from_printings(p: &'a AllPrintings) -> HashMap<String, AtomicCard<'a>> {
        let mut res: HashMap<String, AtomicCard<'a>> = HashMap::new();
        for (expansion_id, expansion) in p.data.iter() {
            for card in expansion.cards.iter() {
                if let Some(atm) = res.get_mut(&card.uuid) {
                    atm.expansions.push(expansion_id.clone());
                } else {
                    res.insert(
                        card.uuid.clone(),
                        AtomicCard {
                            card_data: &card,
                            expansions: vec![expansion_id.clone()],
                        },
                    );
                }
            }
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_printings_works() {
        use std::fs::File;
        use std::io::BufReader;

        if let Ok(file) = File::open("./AllPrintings.json") {
            let reader = BufReader::new(file);

            let deserialized: AllPrintings = serde_json::from_reader(reader).unwrap();
            let atomic_db = AtomicCard::from_printings(&deserialized);
        }
    }
}
