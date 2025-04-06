#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    #[test]
    fn test_parse_and_type_checking_sum_test() {
        let mut checker = cfg_ssa::type_checking::TypeChecker::new();
        let file_path = Path::new("./compiled/sum_test_cvm/sum_test.cvm");

        if file_path.is_file() {
            let content = fs::read_to_string(&file_path).expect("Failed to read file");
            match cvm_parser::parse_program(&content) {
                Ok((_, parsed)) => {
                    println!("Parsed successfully: {:?}", parsed);
                    match checker.check(&parsed) {
                        Ok(_) => println!("Type checking passed for file: {:?}", file_path),
                        Err(e) => panic!("Type checking failed for file {:?}: {:?}", file_path, e),
                    }
                }
                Err(e) => panic!("Parsing failed for file {:?}: {:?}", file_path, e),
            }
        } else {
            panic!("File does not exist: {:?}", file_path);
        }
    }

    #[test]
    fn test_parse_and_type_checking() {
        let mut checker = cfg_ssa::type_checking::TypeChecker::new();
        let compiled_folder = Path::new("./compiled/");
    
        // Process a single file: read, parse, and type check.
        fn process_file(path: &Path, checker: &mut cfg_ssa::type_checking::TypeChecker) {
            let content = match fs::read_to_string(path) {
                Ok(content) => content,
                Err(e) => {
                    eprintln!("Failed to read file {:?}: {:?}", path, e);
                    return;
                }
            };
    
            match cvm_parser::parse_program(&content) {
                Ok((_, parsed)) => {
                    println!("Parsed successfully: {:?}", parsed);
                    match checker.check(&parsed) {
                        Ok(_) => println!("Type checking passed for file: {:?}", path),
                        Err(e) => panic!("Type checking failed for file {:?}: {:?}", path, e),
                    }
                }
                Err(e) => panic!("Parsing failed for file {:?}: {:?}", path, e),
            }
        }
    
        // Recursively process all files in the directory.
        fn process_directory(dir: &Path, checker: &mut cfg_ssa::type_checking::TypeChecker) {
            for entry in fs::read_dir(dir).unwrap_or_else(|e| {
                panic!("Failed to read directory {:?}: {:?}", dir, e);
            }) {
                let entry = entry.expect("Failed to read directory entry");
                let path = entry.path();
                if path.is_dir() {
                    process_directory(&path, checker);
                } else if path.is_file() {
                    process_file(&path, checker);
                }
            }
        }
    
        process_directory(compiled_folder, &mut checker);
    }
    
}
