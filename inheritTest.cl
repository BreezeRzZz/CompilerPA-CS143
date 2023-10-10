Class Main{
    main():Int{
        1
    };
};

Class A{
    a:String;
};

Class B inherits A{
    a:Int;
    b:String;
};

Class C inherits B{
    func():Int{
        a + b
    };
};