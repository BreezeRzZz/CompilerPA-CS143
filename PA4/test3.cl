class Main { main() : Int {0 }; };

class A {
	inky():Int { 1 };
};

class B inherits A {
	binky():String {"hello"};
};

class C {
	b:B;
	winky():Int { b@B.inky() };
};

-- test from grading file
-- static_dispatch should also be able to use the dispatch class's ancestor's methods