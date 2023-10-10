Class Main{
    main():Int {
        1
    };
};

Class B inherits Main{

};

Class C inherits Main{

};

Class Z{
    func(a:Int,b:Int):Int{
        1
    };
};

Class A inherits Z{
    b:B <- new B;
    c:C <- new C;
    -- assign: undefined id(lhs and rhs)
    a():Object{
        a <- a
    };
    b():Object{
        -- assign: not conform
        b <- "ab"
    };
    
    c():Int{
        -- new : undefined class
        -- should be type Object
        new C
    };

    d():Int{
        -- if: pred is not Bool.
        -- here we check the return type is not Object,though pred is wrong
        if 1 then b else c fi  
    };

    e():Int{
        -- arith: incorrect type of LHS or RHS
        -- (block expr cannot be semantically wrong)
        {
            1 + "a";
            "a" - 1;
            "a" * true;
            "a" / 0;        -- divide by zero is not a semantic error
        }
        -- return type check: when error, type is still Int, not Object
    };

    f():Int{
        -- neg : incorrect type of argument
        ~"111"
        -- return type check: when error, type is still Int, not Object
    };

    g():Int{
        -- lt & leq : incorrect type of argument
        {
            1 < "a";
            true <= "b";
        }
        -- return type check: when error, type is still Bool, not Object
    };

    h():Int{
        -- comp : incorrect type of argument
        not 123
    };

    i():Int{
        -- eq: check all combination
        {
            -- 1. both are not Int or Bool or String. should be OK;
            b = c;
            -- 2. one is Int
            1 = b;
            -- 3. one is Int, the other is String
            1 = "a";
        }
        -- return type: when error, type is still Bool, not Object
    };

    j():Int{
        -- loop : check pred type
        while 1 loop b pool
    };

    k():Int{
        -- cases/branch:
        {
            -- 1. check if there are duplicate branches(i.e. the same type declaration,not the id)
            case 1 of
                x:Int => 1;
                x:Int => 2;
                x:Int => new E;
            esac;

            -- 2. check the type of the whole expression
            case 1 of
                x:B => x;
                x:B => 1;
                y:C => y;
            esac;
            -- even the branch is duplicate, its branch type should still be gathered
        }
    };

    l():Bool{
        -- let:
        {
            -- 1. declared type is undefined
            let a:D <- 1 in a;
            -- 2. identifier is 'self'
            let self:D <- 1 in self;
            -- 3. init not conform(note return type is String, i.e. type_decl)
            let a:String <- 1 in a;

            -- return type check????
            -- OK type is still D
            let a:D <- 1 in a;
        }
        + 1
        -- type should be expr's type, unless is undefined
    };

    m():Int{
        -- dispatch:
        {
            -- 1. undeclared id
            a.type_name();
            -- 2. method undefined
            (new A).asdasd(g);
            -- 3. actual not conform to formal
            (new A).func(1,"abc");
            -- 4. acutal num not the same
            (new A).func(1);
        }
    };

    n():Int{
        -- static_dispatch:
        {
            -- 1. class undefined
            (new A)@U.asd(g);
            -- 2. LHS of '@' not conform to RHS
            (new A)@Main.main(g);
            -- 3. undeclared id
            a@Object.type_name();
            -- 4. method undefined
            (new A)@Z.m();
            -- 5. actual not conform to formal
            (new A)@Z.func(g,"abc");
            -- 6. actual num not the same
            (new A)@Z.func(1);
        }
    };
};