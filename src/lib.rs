use std::cmp::Ordering;
use std::fmt;
use std::fs::File;
use std::hash::Hash;
use std::io::{self, BufRead, BufReader};
use std::num::NonZeroU8;
use std::str::FromStr;

/// A book of the Bible.
///
/// This includes the 66 books of the Protestant Bible, plus 20
/// books/additions used in the Catholic and Eastern Orthodox traditions.
// todo: better abbreviations?
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub enum Book {
    /// Genesis
    Gen,
    /// Exodus
    Ex,
    /// Leviticus
    Lev,
    /// Numbers
    Num,
    /// Deuteronomy
    Deut,
    /// Joshua
    Josh,
    /// Judges
    Judg,
    /// Ruth
    Ruth,
    /// 1 Samuel
    I_Sam,
    /// 2 Samuel
    II_Sam,
    /// 1 Kings
    I_Kings,
    /// 2 Kings
    II_Kings,
    /// 1 Chronicles
    I_Chr,
    /// 2 Chronicles
    II_Chr,
    /// Ezra
    Ezra,
    /// Nehemiah
    Neh,
    /// Esther
    Esth,
    /// Job
    Job,
    /// Psalms
    Ps,
    /// Proverbs
    Prov,
    /// Ecclesiastes
    Eccl,
    /// Song of Solomon
    Song,
    /// Isaiah
    Isa,
    /// Jeremiah
    Jer,
    /// Lamentations
    Lam,
    /// Ezekiel
    Ezek,
    /// Daniel
    Dan,
    /// Hosea
    Hos,
    /// Joel
    Joel,
    /// Amos
    Am,
    /// Obadiah
    Ob,
    /// Jonah
    Jon,
    /// Micah
    Mic,
    /// Nahum
    Nah,
    /// Habakkuk
    Hab,
    /// Zephaniah
    Zeph,
    /// Haggai
    Hag,
    /// Zechariah
    Zech,
    /// Malachi
    Mal,
    /// Matthew
    Mt,
    /// Mark
    Mk,
    /// Luke
    Lk,
    /// John
    Jn,
    /// Acts
    Acts,
    /// Romans
    Rom,
    /// 1 Corinthians
    I_Cor,
    /// 2 Corinthians
    II_Cor,
    /// Galatians
    Gal,
    /// Ephesians
    Eph,
    /// Philippians
    Phil,
    /// Colossians
    Col,
    /// 1 Thessalonians
    I_Thess,
    /// 2 Thessalonians
    II_Thess,
    /// 1 Timothy
    I_Tim,
    /// 2 Timothy
    II_Tim,
    /// Titus
    Titus,
    /// Philemon
    Philem,
    /// Hebrews
    Heb,
    /// James
    Jas,
    /// 1 Peter
    I_Pet,
    /// 2 Peter
    II_Pet,
    /// 1 John
    I_Jn,
    /// 2 John
    II_Jn,
    /// 3 John
    III_Jn,
    /// Jude
    Jude,
    /// Revelation
    Rev,
    /// Tobit
    Tob,
    /// Judith
    Jdt,
    /// Greek additions to Esther
    EsthGrk,
    /// Wisdom of Solomon
    Wis,
    /// Sirach
    Sir,
    /// Baruch
    Bar,
    /// Letter of Jeremiah
    LetJer,
    /// Prayer of Azariah
    SongOfThr,
    /// Susanna
    Sus,
    /// Bel and the Dragon
    Bel,
    /// 1 Maccabees
    I_Macc,
    /// 2 Maccabees
    II_Macc,
    /// 3 Maccabees
    III_Macc,
    /// 4 Maccabees
    IV_Macc,
    /// 1 Esdras
    I_Esd,
    /// 2 Esdras
    II_Esd,
    /// Prayer of Manasses
    PrMan,
    /// Psalm 151
    Ps151,
    /// Psalm of Solomon
    PsSol,
    /// Odes
    Odes,
}

impl Book {
    fn new(s: &str) -> Option<Self> {
        match s {
            "01O" => Some(Self::Gen),
            "02O" => Some(Self::Ex),
            "03O" => Some(Self::Lev),
            "04O" => Some(Self::Num),
            "05O" => Some(Self::Deut),
            "06O" => Some(Self::Josh),
            "07O" => Some(Self::Judg),
            "08O" => Some(Self::Ruth),
            "09O" => Some(Self::I_Sam),
            "10O" => Some(Self::II_Sam),
            "11O" => Some(Self::I_Kings),
            "12O" => Some(Self::II_Kings),
            "13O" => Some(Self::I_Chr),
            "14O" => Some(Self::II_Chr),
            "15O" => Some(Self::Ezra),
            "16O" => Some(Self::Neh),
            "17O" => Some(Self::Esth),
            "18O" => Some(Self::Job),
            "19O" => Some(Self::Ps),
            "20O" => Some(Self::Prov),
            "21O" => Some(Self::Eccl),
            "22O" => Some(Self::Song),
            "23O" => Some(Self::Isa),
            "24O" => Some(Self::Jer),
            "25O" => Some(Self::Lam),
            "26O" => Some(Self::Ezek),
            "27O" => Some(Self::Dan),
            "28O" => Some(Self::Hos),
            "29O" => Some(Self::Joel),
            "30O" => Some(Self::Am),
            "31O" => Some(Self::Ob),
            "32O" => Some(Self::Jon),
            "33O" => Some(Self::Mic),
            "34O" => Some(Self::Nah),
            "35O" => Some(Self::Hab),
            "36O" => Some(Self::Zeph),
            "37O" => Some(Self::Hag),
            "38O" => Some(Self::Zech),
            "39O" => Some(Self::Mal),
            "40N" => Some(Self::Mt),
            "41N" => Some(Self::Mk),
            "42N" => Some(Self::Lk),
            "43N" => Some(Self::Jn),
            "44N" => Some(Self::Acts),
            "45N" => Some(Self::Rom),
            "46N" => Some(Self::I_Cor),
            "47N" => Some(Self::II_Cor),
            "48N" => Some(Self::Gal),
            "49N" => Some(Self::Eph),
            "50N" => Some(Self::Phil),
            "51N" => Some(Self::Col),
            "52N" => Some(Self::I_Thess),
            "53N" => Some(Self::II_Thess),
            "54N" => Some(Self::I_Tim),
            "55N" => Some(Self::II_Tim),
            "56N" => Some(Self::Titus),
            "57N" => Some(Self::Philem),
            "58N" => Some(Self::Heb),
            "59N" => Some(Self::Jas),
            "60N" => Some(Self::I_Pet),
            "61N" => Some(Self::II_Pet),
            "62N" => Some(Self::I_Jn),
            "63N" => Some(Self::II_Jn),
            "64N" => Some(Self::III_Jn),
            "65N" => Some(Self::Jude),
            "66N" => Some(Self::Rev),
            "67A" => Some(Self::Tob),
            "68A" => Some(Self::Jdt),
            "69A" => Some(Self::EsthGrk),
            "70A" => Some(Self::Wis),
            "71A" => Some(Self::Sir),
            "72A" => Some(Self::Bar),
            "73A" => Some(Self::LetJer),
            "74A" => Some(Self::SongOfThr),
            "75A" => Some(Self::Sus),
            "76A" => Some(Self::Bel),
            "77A" => Some(Self::I_Macc),
            "78A" => Some(Self::II_Macc),
            "79A" => Some(Self::III_Macc),
            "80A" => Some(Self::IV_Macc),
            "81A" => Some(Self::I_Esd),
            "82A" => Some(Self::II_Esd),
            "83A" => Some(Self::PrMan),
            "84A" => Some(Self::Ps151),
            "85A" => Some(Self::PsSol),
            "86A" => Some(Self::Odes),
            _ => None,
        }
    }

    #[must_use]
    pub const fn as_str(&self) -> &str {
        match self {
            Self::Gen => "Genesis",
            Self::Ex => "Exodus",
            Self::Lev => "Leviticus",
            Self::Num => "Numbers",
            Self::Deut => "Deuteronomy",
            Self::Josh => "Joshua",
            Self::Judg => "Judges",
            Self::Ruth => "Ruth",
            Self::I_Sam => "1 Samuel",
            Self::II_Sam => "2 Samuel",
            Self::I_Kings => "1 Kings",
            Self::II_Kings => "2 Kings",
            Self::I_Chr => "1 Chronicles",
            Self::II_Chr => "2 Chronicles",
            Self::Ezra => "Ezra",
            Self::Neh => "Nehemiah",
            Self::Esth => "Esther",
            Self::Job => "Job",
            Self::Ps => "Psalms",
            Self::Prov => "Proverbs",
            Self::Eccl => "Ecclesiastes",
            Self::Song => "Song of Solomon",
            Self::Isa => "Isaiah",
            Self::Jer => "Jeremiah",
            Self::Lam => "Lamentations",
            Self::Ezek => "Ezekiel",
            Self::Dan => "Daniel",
            Self::Hos => "Hosea",
            Self::Joel => "Joel",
            Self::Am => "Amos",
            Self::Ob => "Obadiah",
            Self::Jon => "Jonah",
            Self::Mic => "Micah",
            Self::Nah => "Nahum",
            Self::Hab => "Habakkuk",
            Self::Zeph => "Zephaniah",
            Self::Hag => "Haggai",
            Self::Zech => "Zechariah",
            Self::Mal => "Malachi",
            Self::Mt => "Matthew",
            Self::Mk => "Mark",
            Self::Lk => "Luke",
            Self::Jn => "John",
            Self::Acts => "Acts",
            Self::Rom => "Romans",
            Self::I_Cor => "1 Corinthians",
            Self::II_Cor => "2 Corinthians",
            Self::Gal => "Galatians",
            Self::Eph => "Ephesians",
            Self::Phil => "Philippians",
            Self::Col => "Colossians",
            Self::I_Thess => "1 Thessalonians",
            Self::II_Thess => "2 Thessalonians",
            Self::I_Tim => "1 Timothy",
            Self::II_Tim => "2 Timothy",
            Self::Titus => "Titus",
            Self::Philem => "Philemon",
            Self::Heb => "Hebrews",
            Self::Jas => "James",
            Self::I_Pet => "1 Peter",
            Self::II_Pet => "2 Peter",
            Self::I_Jn => "1 John",
            Self::II_Jn => "2 John",
            Self::III_Jn => "3 John",
            Self::Jude => "Jude",
            Self::Rev => "Revelation",
            Self::Tob => "Tobit",
            Self::Jdt => "Judith",
            Self::EsthGrk => "Esther, Greek",
            Self::Wis => "Wisdom of Solomon",
            Self::Sir => "Sirach",
            Self::Bar => "Baruch",
            Self::LetJer => "Epistle of Jeremiah",
            Self::SongOfThr => "Prayer of Azariah",
            Self::Sus => "Susanna",
            Self::Bel => "Bel and the Dragon",
            Self::I_Macc => "1 Maccabees",
            Self::II_Macc => "2 Maccabees",
            Self::III_Macc => "3 Maccabees",
            Self::IV_Macc => "4 Maccabees",
            Self::I_Esd => "1 Esdras",
            Self::II_Esd => "2 Esdras",
            Self::PrMan => "Prayer of Manasseh",
            Self::Ps151 => "Psalm 151",
            Self::PsSol => "Psalm of Solomon",
            Self::Odes => "Odes",
        }
    }
}

impl fmt::Display for Book {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

/// The location of a single verse in the NRSV Bible, as a book, chapter, and verse number.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct PositionNrsv {
    book: Book,
    chap_no: u16,
    vers_no: u16,
}

impl PositionNrsv {
    /// The position's `Book`.
    #[must_use]
    pub const fn book(&self) -> Book {
        self.book
    }

    /// The position's chapter number.
    #[must_use]
    pub const fn chap_no(&self) -> u16 {
        self.chap_no
    }

    /// The position's verse number.
    #[must_use]
    pub const fn vers_no(&self) -> u16 {
        self.vers_no
    }
}

impl fmt::Display for PositionNrsv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.book == Book::Ps151 {
            write!(f, "Psalm 151:{}", self.vers_no)
        } else {
            write!(f, "{} {}:{}", self.book, self.chap_no, self.vers_no)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum PositionMeta {
    None,
    Subverse(NonZeroU8),
    VerseRange(NonZeroU8),
}

impl PositionMeta {
    fn new(s: &str) -> Option<Self> {
        if s.is_empty() {
            Some(Self::None)
        } else if let Some(stripped) = s.strip_prefix('-') {
            let x = NonZeroU8::from_str(stripped)
                .unwrap_or_else(|_e| panic!("cannot parse {:?} as verse range", s));
            Some(Self::VerseRange(x))
        } else if let Some(stripped) = s.strip_prefix('.') {
            let x = NonZeroU8::from_str(stripped)
                .unwrap_or_else(|_e| panic!("cannot parse {:?} as subverse", s));
            Some(Self::Subverse(x))
        } else {
            let x = match s {
                "a" | "EndA" => 1,
                "b" | "EndB" => 2,
                "c" => 3,
                "d" => 4,
                "e" => 5,
                "f" => 6,
                "g" => 7,
                "h" => 8,
                "i" => 9,
                "j" => 10,
                "k" => 11,
                "l" => 12,
                "m" => 13,
                "n" => 14,
                "o" => 15,
                "p" => 16,
                "q" => 17,
                "r" => 18,
                "s" => 19,
                "t" => 20,
                "u" => 21,
                "v" => 22,
                "w" => 23,
                "x" => 24,
                "y" => 25,
                "z" => 26,
                "aa" => 27,
                "bb" => 28,
                "cc" => 29,
                "dd" => 30,
                "ee" => 31,
                "ff" => 32,
                "gg" => 33,
                "hh" => 34,
                "ii" => 35,
                "jj" => 36,
                "kk" => 37,
                _ => return None,
            };
            Some(Self::Subverse(NonZeroU8::new(x).unwrap()))
        }
    }
}

/// The location of (roughly) verse-sized chunk of an arbitrary Bible translation.
/// In the Unbound file format, these usually correspond to a single NRSV verse.
///
/// Unlike `PositionNRSV`, this type is translation-specific.
/// Comparisons between `Position`s only make sense within a single translation.
/// Otherwise, the behavior is what you would expect for `PositionNRSV`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Position {
    book: Book,
    chap_no: u16,
    vers_no: u16,
    meta: PositionMeta,
}

impl Position {
    /// The position's `Book`.
    #[must_use]
    pub const fn book(&self) -> Book {
        self.book
    }

    /// The position's chapter number.
    #[must_use]
    pub const fn chap_no(&self) -> u16 {
        self.chap_no
    }

    /// The position's starting verse number.
    ///
    /// For a position representing a verse range, this is the starting verse number.
    /// Otherwise, it is the same as `PositionNRSV`'s `vers_no`.
    #[must_use]
    pub const fn vers_beg(&self) -> u16 {
        self.vers_no
    }

    /// The position's ending verse number.
    ///
    /// For a position representing a verse range, this is the verse number of the last verse in the range.
    /// Otherwise, it is the same as `PositionNRSV`'s `vers_no`.
    #[must_use]
    pub const fn vers_end(&self) -> u16 {
        let delta = if let PositionMeta::VerseRange(delta) = self.meta {
            delta.get() as _
        } else {
            0
        };
        self.vers_no + delta
    }

    /// The position's subverse.
    ///
    /// For a position representing a subverse, this will return `Some(subverse)`.
    /// Subverses are numbered `1` through `N`.
    /// Currently, `N` does not appear to exceed `37` for any translation.
    #[must_use]
    pub const fn subverse(&self) -> Option<NonZeroU8> {
        if let PositionMeta::Subverse(x) = self.meta {
            Some(x)
        } else {
            None
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (book, chap_no) = if self.book == Book::Ps151 {
            ("Psalm 151", 151)
        } else {
            (self.book.as_str(), self.chap_no)
        };
        write!(f, "{} {}:{}", book, chap_no, self.vers_no)?;
        match self.meta {
            PositionMeta::Subverse(x) => write!(f, ".{}", x),
            PositionMeta::VerseRange(x) => write!(f, "-{}", x),
            PositionMeta::None => Ok(()),
        }
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        let ordering = self
            .book
            .cmp(&rhs.book)
            .then(self.chap_no.cmp(&rhs.chap_no))
            .then(self.vers_no.cmp(&rhs.vers_no));
        match (self.meta, rhs.meta) {
            (PositionMeta::None, PositionMeta::None) => Some(ordering),
            (PositionMeta::Subverse(ref lhs), PositionMeta::Subverse(ref rhs))
            | (PositionMeta::VerseRange(ref lhs), PositionMeta::VerseRange(ref rhs)) => {
                Some(ordering.then(lhs.cmp(rhs)))
            }
            _ => None,
        }
    }
}

/// A single row from a file in the Unbound format.
///
/// For a translation that closely follows the NRSV numbering scheme,
/// this is a single verse.
/// Otherwise, it can possibly be a range of verses or a subverse.
#[derive(Debug)]
pub struct Verse {
    pos_nrsv: Option<PositionNrsv>,
    pos_orig: Position,
    text: String,
}

impl Verse {
    /// The text of the line or verse.
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The position of the line or verse.
    #[must_use]
    pub const fn pos(&self) -> Position {
        self.pos_orig
    }

    /// The corresponding position of the line or verse in the NRSV translation.
    #[must_use]
    pub const fn pos_nrsv(&self) -> Option<PositionNrsv> {
        self.pos_nrsv
    }
}

/// An interator over the lines of a file in the Unbound format.
///
/// For a translation that closely follows the NRSV numbering scheme,
/// this yields individual verses.
/// Otherwise, it can possibly yield ranges of verses or subverses,
/// and it can yield the same verse multiple times,
/// albeit with different native positions or NRSV positions.
pub struct Verses<B>(io::Lines<B>);

impl<B: BufRead> Iterator for Verses<B> {
    type Item = io::Result<Verse>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let row = match self.0.next()? {
                Ok(row) => row,
                Err(e) => return Some(Err(e)),
            };
            if row.trim().is_empty() || row.starts_with('#') {
                continue;
            }
            let mut cols = row.split('\t');
            let pos_nrsv = match (
                cols.next().unwrap(),
                cols.next().unwrap(),
                cols.next().unwrap(),
            ) {
                ("", "", "") => None,
                (x, y, z) => Some(PositionNrsv {
                    book: Book::new(x).unwrap(),
                    chap_no: u16::from_str(y).unwrap(),
                    vers_no: u16::from_str(z).unwrap(),
                }),
            };
            let (col4, col5, col6, col7, col8) = (
                cols.next().unwrap(),
                cols.next().unwrap(),
                cols.next().unwrap(),
                cols.next().unwrap(),
                cols.next().unwrap(),
            );
            if Ok(0) == u8::from_str(col8) {
                continue;
            }
            let pos_orig = Position {
                book: Book::new(col4).unwrap(),
                chap_no: u16::from_str(col5).unwrap(),
                vers_no: u16::from_str(col6).unwrap(),
                meta: PositionMeta::new(col7).unwrap(),
            };
            match cols.next() {
                Some(x) if !x.is_empty() => {
                    assert!(cols.next().is_none());
                    return Some(Ok(Verse {
                        pos_nrsv,
                        pos_orig,
                        text: x.into(),
                    }));
                }
                _ => continue,
            }
        }
    }
}

impl<B: BufRead> Verses<B> {
    fn new(b: B) -> Self {
        Self(b.lines())
    }
}

/// Create an iterator over the lines of a string in the Unbound format.
#[must_use]
pub fn from_str(s: &str) -> Verses<&[u8]> {
    Verses::new(s.as_ref())
}

/// Create an iterator over the lines of a file in the Unbound format.
#[must_use]
pub fn from_file(f: File) -> Verses<BufReader<File>> {
    Verses::new(BufReader::new(f))
}
