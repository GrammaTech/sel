fn main() {
    match i {
        0 | 2 | 4 => {
            output = String::from("even");
        }
        1 | 3 | 5 => {
            output = String::from("odd");
        }
        -1 => {
            output = String::from("negative");
        }
        _ => {
            output = String::from("?");
        }
    };
}
