Class Main{
    main():Int{
        1
    };
};

Class A{
    x:Int;
    y:String;
    z:Bool;
    a():Int{
        1
    };
};

Class B inherits A{
    a():Int{
        -- 1. should be able to use parent class's attrs and methods
        {
            x <- 1;
            y <- "abc";
            z <- true;

            x <- a();       -- a strange test? formal num ...
        }
    };

    b():Int{
        -- 2. let test
        let x : Int, value : Int <- x + 1 in let value : Int <- 1 in x + value
    };

    c():Int{
        -- 3. case test
        case 1 of
            y:Int => y + 1;        -- should hide attr y
            a:Bool => 1;
            -- b:String => a;      -- a should be undeclared
        esac
    };

    -- 4. method test

    -- should hide attr x and y
    d(x:String,y:Int):Int {
        {
            x <- "abc";
            y <- 1;
        }
    };
};