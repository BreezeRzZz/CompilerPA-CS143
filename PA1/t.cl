Class Main inherits A2I{
    x:Int <- 001;
    y:Int <- 0002;
    io:IO <- new IO;
    main():Object{
        io.out_string(i2a(x+y).concat("\n"))
    };
};
