Class Main{
    main():Int{
        1
    };
};

Class A{
    x:SELF_TYPE;
    self:Int;
    a():SELF_TYPE{
        new A       -- should not conform(notice)
    };

    b():A{
        self        -- conform
    };

    c():SELF_TYPE{  -- conform
        new SELF_TYPE
    };
};

Class B inherits A{
    self:String;            -- self as attr
    d():SELF_TYPE{
        new B       -- should not conform
    };

    e():A{
        self
    };

    f():B{
        self
    };

    -- we should notice:this is only static analysis
    -- so dispatch only considers the return type of the function
    g():B{
        (new A).c()     -- A not conform B
    };

    h():B{
        (new B).c()     --  B conforms B
    };

    i():SELF_TYPE{
        (new A).c()     -- A not conform SELF_TYPE
    };

    j():SELF_TYPE{
        (new B).c()     -- B not conform SELF_TYPE
    };

    k():B{
        self.c()        -- SELF_TYPE conform B
    };

    m():SELF_TYPE{
        self.c()        -- should conform(SELF_TYPE conform SELF_TYPE)
                       -- detail of typing rule: the dispatch type is T_0,not T_0'(i.e. don't convert)
    };

    n():SELF_TYPE{      -- SELF_TYPE
        x
    };
    
    o():Int{
        let a:SELF_TYPE in 
            if true then self else self fi        -- LCA of SELF_TYPE and SELF_TYPE is SELF_TYPE
    };

    p():Int{
        case 1 of
            x:Int => self;
            y:String => self;       -- same as above
            z:Bool => self;
        esac
    };

    q(self:Int):Int{        -- self as formal
        self
    };

    r():String{
        let self:Int in self        -- self as let id:still SELF_TYPE
    };

    s():Bool{
        case 1 of
            self:Int => self;       -- self is Int now
        esac
    };
};