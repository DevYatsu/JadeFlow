use std::{fs, io::Error, path::PathBuf};

use dialoguer::{theme::ColorfulTheme, Select};

pub fn run_file() -> Result<PathBuf, Error> {
    let mut directory_path = String::from("tests/");

    let mut files = match get_files_in(&directory_path) {
        Ok(f) => f,
        Err(e) => return Err(e),
    };
    files.sort();

    let index = Select::with_theme(&ColorfulTheme::default())
        .with_prompt("Select a file for testing")
        .items(&files)
        .interact()
        .unwrap();

    directory_path.push_str(&files[index]);

    Ok(PathBuf::from(directory_path))
}

fn get_files_in(dir: &str) -> Result<Vec<String>, Error> {
    let mut file_names = vec![];
    let entries = fs::read_dir(dir)?;
    for entry in entries {
        if let Ok(entry) = entry {
            let file_path = entry.path();

            if file_path.is_file() {
                // Obtenir le nom du fichier en tant que chaîne
                if let Some(file_name) = file_path.file_name() {
                    if let Some(file_name_str) = file_name.to_str() {
                        file_names.push(file_name_str.to_owned())
                    }
                }
            }
        }
    }
    Ok(file_names)
}
