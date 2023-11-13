use std::collections::BTreeSet;

fn main() -> shadow_rs::SdResult<()> {
    let mut exclude = BTreeSet::new();
    exclude.insert("COMMIT_EMAIL");
    shadow_rs::new_deny(exclude)
}
