use std::fs::File;

fn hline() {
    println!("----------------------------------------");
}

fn main() {
    let f = File::open("kjv_apocrypha_utf8_mapped_to_NRSVA.txt").unwrap();
    let verses = unbound::from_file(f);
    for verse in verses {
        let verse = &verse.unwrap();
        let text = verse.text();
        if text.to_lowercase().contains("son of david") {
            hline();
            println!("{}\n * {} (KJV)", text, verse.pos());
        }
    }
    hline();
}
