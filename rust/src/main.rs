mod lexer;
mod repl;

use repl::repl::start;
fn main() {
    println!("Welcome to the Monkey Programming language");
    println!("Type CTRL+C to exit: \n");
    start();
}
