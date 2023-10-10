
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

-- error : keyword Class is misspelled
Crass t{

};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

-- error : no semicon(offical parser won't report this error)
class E {

}

-- error:  closing brace is missing 
Class E inherits A {
;

-- error : feature error
class E {
    -- attr
    A : Int;            -- A is not object ID
    e : Int <- 1;
    C : int;            -- int is not type ID
    f : Int <- 1;
    -- method
    D():Int{            -- D is not object ID

    };
    -- e : Int             -- no semicon, it is strange... official parser seems ignore 
};

-- formal test
class F {
    f(Int a,b:Int,c:Int <- 1,d:Int):Int{        -- 1. incorrect formal style 2. cannot assign initial value
        1+1
        A+B
    };
};

class Test{
    let_test():Int {
        let A in let b:Int,c,d:Int,e in let c:Int in let d:Int <- ? in c
    };

    -- next expression
    block_test():Int {
        {
            1+1;A+B;
            2+2;
            C+D;
        }
    };

    -- see next feature
    case_test():Int {
        case a of
            b : Int => b;c : Int -> c;
            d : Int => d;           
            e : int => e;
        esac
    };

    -- only first error,then next feature
    dispatch_test():Int {
        f(?;b,c;d)
        f@B(?)
        g(?)
        e(?)
    };

    -- only first error,then next feature
    expr_test():Int {
        A+B
        C+D
    };
};
