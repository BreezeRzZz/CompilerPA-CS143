Class Main{
    main():Int{
        1
    };
};

Class B inherits A{
    func():String{      -- here only installed C's methods.so no problem...
        "ab"
    };
    func():String{
        "ab"
    };
    a:Int;
};

Class A{
    func():Int{
        1
    };
    
};