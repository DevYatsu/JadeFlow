pub fn get_line(position: usize, source_code: &str) -> usize {
    let mut line_number = 1;
    let lines_before_position = &source_code[..position];
    line_number += lines_before_position.matches('\n').count();
    line_number
}
