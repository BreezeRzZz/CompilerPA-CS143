-- inherits from an undefined class
Class A inherits B{

};

-- inherits from Int/Bool/Str/SELF_TYPE
Class C inherits SELF_TYPE{

};

-- class name is SELF_TYPE
Class SELF_TYPE{

};

-- redefintion
Class C{
    func():INT{
        1
    };
};

Class Int{

};

Class Int{

};

Class String{

};

Class Bool{

};

Class IO{

};

Class Object{

};

Class SELF_TYPE{

};