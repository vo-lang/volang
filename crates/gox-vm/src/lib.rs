pub struct VM;

impl VM {
    pub fn new() -> Self {
        VM
    }

    pub fn run(&self) {
        println!("Running Gox VM...");
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
