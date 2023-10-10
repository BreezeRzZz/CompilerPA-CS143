Class Main{
    main():Int{
        1
    };
};


-- important sample
Class C{
    func():Bool{
        true
    };
};

Class B inherits A{
    func():String{     
        "ab"
    };
    a:Int;
};

Class A inherits C{         -- not installed
    func():Int{
        1
    };
    
};

Class E{
    a:INT;
};

Class F inherits E{
    a:Int;
};