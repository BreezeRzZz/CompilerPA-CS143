class A {
    -- attributes test:
    a : String;         -- no initialize
    b : Int <- 1;       -- initialize
    -- methods test
    func1() : Int {     -- no formal
        1
    };

    func2(x:Int) : Int {        -- one formal
        2
    };

    func3(x:Int,y:Int,z:Int) : Int{
        3                       -- multi formals
    };
};

-- inherits test
class B inherits A {
    func3():Int {
        4
    };
};

class C {
    b : B <- new B;
    func4():Int {
        4
    };

    dispatch_test():Int {
        b.func3()
    };

    static_dispatch_test():Int {
        b@A.func3(1,2,3)
    };

    self_dispatch_test():Int {
        func4()
    };

    
    if_then_else_test():Int {
        if 1 < 2  then 1 + 2 else 2 - 1 fi
    };

    -- arithmatic and comparison tests included
    if_then_else_nest_test(a:Int,b:Int):Int {
        if a < b then a + b
        else if b < a then a - b
        else if a = b then a * b
        else a / b
        fi fi fi
    };

    while_test(a:Int):Int {
        while a < 10
        loop a <- a + 1 
        pool
    };

    block_test():Int {
        {
            1 + 1;
            2 - 1;
            10;
        }
    };

    block_nest_test():Int {
        {
            {
                {
                    1;
                };
            };
            {
                2;
            };
            3;
        }
    };

    let_test():Int {
        let a:Int in a
    };

    let_nest_test():Int {
        let a:Int <-3 in let b:Int <-1 in let c:Int<-2,d:Int in a+b+c+d 
    };

    case_test(x:Int):Int {
        case x of
            a : Int => a;
            b : String => 12;
        esac
    };

    new_test():Int {
        new Int
    };

    isvoid_test(a:Int):Int {
        isvoid(a)
    };

    logicnot_test(a:Int ):Int {
        ~a
    };

    le_test(): Int {
        1 <= 2
    };

    not_test(): Int {
        not 1
    };

    bracket_test(): Int {
        (1)
    };

    string_test(): String {
        "Hello world\n"
    };

    boolean_test(a: Int): Boolean {
        if a = 1 then true else false fi
    };

    multilines_test():Int {
        {
            1
            +
            1;
            1
            -
            1;
            1
            *
            1;
            1
            /
            1;
            ~
            1;
            1
            <
            1;
            1
            <=
            1;
            1
            =
            1;
            not
            1;
            (
                1
            );
            isvoid
            1;
            new 
            A;
        }
    };
};