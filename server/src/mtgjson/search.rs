
enum Query {
    Color {
        Contain(Vec<Color>),
        Match(Vec<Color>),
        DontContain(Vec<Color>),
    },
    ColorIdentity {
        Contain(Vec<Color>),
        Match(Vec<Color>),
        DontContain(Vec<Color>),
    }
    ManaCost {
        Castable(ManaCost),
        Partial(ManaCost),
        Equal(ManaCost)
    },
    ConvertedManaCost {
        GreaterEq(i8),
        LessEq(i8),
        Equal(i8)
    },
    Legality {
        Legal(String),
        Illegal(String),
    },
    Leadership {
        Legal(String),
        Illegal(String),
    },
    Power {
        GreaterEq(i8),
        LessEq(i8),
        Equal(i8)
    },
    Toughness {
        GreaterEq(i8),
        LessEq(i8),
        Equal(i8)
    },
    Name {
        Contains(String),
        Match(String),
        NotMatch(String),
    },
    Text {
        Contains(String),
        Match(String),
        NotMatch(String),
    },
    FlavorText {
        Contains(String),
        Match(String),
        NotMatch(String),
    },
}
