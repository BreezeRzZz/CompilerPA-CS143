Class Main{
    main():Int{
        1
    };
};

Class A{
    -- decl_type undefined
    a:INT <- c;
    -- not conform
    b:Int <- "abc";
    -- redefinition
    b:String;
};

Class B inherits A{
    -- inherit attribute redefinition
    -- we can guess that the traverse order of official implementation is different.
    a:Int;055
    a:Int;
};