class Main {
  main():IO {
   (new IO).out_string((new A).func(1,2,3))
  };
};

Class B{
   z:Int <- 1;
   test():Int{
      1
   };
};

Class A inherits B{
   a:Int <- 2;
   b:String <- "abc";
   c:Bool;
   func(x:Int,y:Int,z:Int):String{
         let x:String <- "abc" in {
            let x:String <- "ab" in x;
            x;
         }
   };
};