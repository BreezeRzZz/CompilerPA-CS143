(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Main inherits IO 
{
   sc : StackCommand <- new StackCommand;
   isStop : Bool <- false;
   main() : Object 
   {
      while not isStop loop
      {
         out_string(">");
         let ch : String <- in_string() in 
         (
            if ch = "e" then sc.evaluate()
            else if ch = "x" then isStop <- true
            else if ch = "d" then sc.display()
            else sc.push(ch)
            fi fi fi
         );
      } pool
   };
};

class StackCommand inherits A2I
{ 
   stack : String;
   io : IO <- new IO;

   push(char : String) : SELF_TYPE 
   {
      {
         stack <- stack.concat(char);
         self;
      }
   };

   display(): SELF_TYPE 
   {
      {
         let n : Int <- stack.length() - 1 in 
         (
            while 0 <= n loop 
            {
               io.out_string(stack.substr(n,1));
               io.out_string("\n");
               n <- n - 1;
            } pool
         );
         self;
      }
   };

   evaluate(): SELF_TYPE 
   {
      {
         let n: Int <- stack.length() in 
         (
            if n = 0 then {self;}
            else if stack.substr(n - 1, 1) = "+" then
            {
               let x : Int <- a2i(stack.substr(n - 3, 1)), y: Int <- a2i(stack.substr(n - 2, 1)) in
               (
                  let ans : String <- i2a(x + y) in
                     stack <- stack.substr(0, n - 3).concat(ans)
               );
            }
            else if stack.substr(n - 1, 1) = "s" then
            {
               let x: String <- stack.substr(n - 3, 1), y: String <- stack.substr(n - 2, 1) in
               (
                  let ans : String <- y.concat(x) in
                     stack <- stack.substr(0, n - 3).concat(ans)
               );
            }
            else {self;}
            fi fi fi
         );         
         self;
      }
   };
};
