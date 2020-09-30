use serde::{Deserialize, Serialize};

use super::mana::ManaCost;
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
enum Color {
    W,
    U,
    B,
    R,
    G,
}

#[derive(Serialize, Deserialize)]
struct Foreign {
    language: String,
    name: String,
    text: Option<String>,

    #[serde(rename = "type")]
    type_text: Option<String>,
}

#[serde(rename_all = "camelCase")]
#[derive(Serialize, Deserialize)]
struct AtomicCardData {
    color_identity: Vec<Color>,
    colors: Vec<Color>,
    converted_mana_cost: f32,
    foreign_data: Vec<Foreign>,
    legalities: HashMap<String, String>,

    #[serde(default = "default_leadership_skills")]
    leadership_skills: HashMap<String, bool>,
    mana_cost: Option<ManaCost>,

    power: Option<String>,
    toughness: Option<String>,

    name: String,
    text: Option<String>,

    supertypes: Vec<String>,
    types: Vec<String>,
    subtypes: Vec<String>,

    identifiers: HashMap<String, String>,
    //    keywords: Vec<String>,
}
fn default_leadership_skills() -> HashMap<String, bool> {
    HashMap::new()
}

#[derive(Serialize, Deserialize)]
struct AtomicCard {
    data: HashMap<String, Vec<AtomicCardData>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        use std::fs::File;
        use std::io::BufReader;

        if let Ok(file) = File::open("./AtomicCards.json") {
            let reader = BufReader::new(file);

            let deserialized: AtomicCard = serde_json::from_reader(reader).unwrap();
        }
    }
}
