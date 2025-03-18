use std::cell::RefCell;

struct Person<'ctx> {
    age: u32,
    name: String,
    friends: RefCell<Vec<&'ctx Person<'ctx>>>,
}

impl<'ctx> Person<'ctx> {
    fn new(age: u32, name: impl Into<String>) -> Self {
        Self {
            age,
            name: name.into(),
            friends: Default::default(),
        }
    }

    fn add_friend(&self, person: &'ctx Person<'ctx>) {
        self.friends.borrow_mut().push(person);
    }

    fn print(&self) {
        print!("Hi, I'm {}, and I'm {} years old. These are my friends: ", self.name, self.age);
        for f in self.friends.borrow().iter() {
            print!("{}, ", f.name);
        }
        println!();
    }
}

arena::define_arenas!(
    people: Person<'ctx>,
);

pub fn main() {
    let arena = Arena::new();

    let luke = Person::new(12, "Luke").alloc_into(&arena);
    let susan = Person::new(15, "Susan").alloc_into(&arena);
    let laura = Person::new(8, "Laura").alloc_into(&arena);
    let alejandro = Person::new(28, "Alejandro").alloc_into(&arena);

    luke.add_friend(susan);
    luke.add_friend(alejandro);
    alejandro.add_friend(laura);
    laura.add_friend(luke);
    susan.add_friend(luke);

    luke.print();
    susan.print();
    laura.print();
    alejandro.print();
}
