
fn main() {
    lalrpop::process_root().unwrap();
    // unwrap: 当Result是Ok的时候, 返回值; 当Result时Err的时候, 调用panic!
}