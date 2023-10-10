class Main{
    main():Int{
        1
    };
};

class A{
    func(a:Int,b:Bool):Int{
        1
    };
};

-- invalid overrides
-- first check return type!
Class B inherits A{
    func(a:Int,b:String):String{
        "abc"
    };
};

-- if you put a 'self' while override,or redefined formal name...?
-- it seems the official semant may implement by different functions.(may traverse 2 times!).That doesn't matter!
Class C inherits A{
    func(a:Int,b:String,c:Int):String{
        "abc"
    };
};

-- then func num
Class D inherits A{
    func(a:Int,b:String,c:Int):Int{
        1
    };
};

-- finally formal type
Class E inherits A{
    func(b:Bool,a:Int):Int{
        1
    };
};

Class P inherits A{
    func():Int{
        1
    };
    func():Int{
        1
    };
};


class F{
    -- method has a formal name 'self'
    -- notice won't show redefinition error
    a(self:Int,self:Int):Int{
        self
    };
    -- SELF_TYPE
    f(self:SELF_TYPE):Int{
        1
    };
    -- formal redefinition
    b(b:Int,b:Bool):Int{
        b
    };
    -- formal type is undefined
    -- we find that when decl_type undefined, go on next check(i.e. is it self?), not check next formal
    c(x:INT,x:INT,self:STR):Int{
        1
    };
    -- return type is undefined
    -- indicates that return type is checked after formal checks
    d(self:Int,self:INT):INT{
        1
    };

    -- expr type does not conform to return type
    e(self:Int,self:INT):Int{
        "abc"
    };

    -- redefinition(Cool does not support override in one class)
    e():Int{
        1
    };
};