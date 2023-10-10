-- inheritance cycle
Class E inherits A{

};
class A inherits B{

};

class C inherits A{

};

class B inherits C{

};

class D inherits D{

};

class Main{
    main():Int {
        1
    };
};