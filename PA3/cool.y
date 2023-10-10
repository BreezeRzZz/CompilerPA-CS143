/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    %type <expression> expr
    %type <expressions> dispatch_exprs
    %type <expressions> block_exprs
    %type <expression> let_expr
    %type <case_> case
    %type <cases> case_list
    %type <formal> formal
    %type <formals> formal_list
    
    /* You will want to change the following line. */
    %type <features> feature_list
    %type <feature>  feature

    /* Precedence declarations go here. */
    /* from top to bottom, the priority is getting higher*/

    %right IN
    %right ASSIGN
    %right NOT
    %nonassoc LE '<' '='
    %left '+' '-'
    %left '*' '/'
    %left ISVOID
    %left '~'
    %left '@'
    %left '.'
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    // example : class A class B ...
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list
    : class			/* single class */
    { $$ = single_Classes($1);
    parse_results = $$; }
    | class_list class	/* several classes */
    { $$ = append_Classes($1,single_Classes($2)); 
    parse_results = $$; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	: CLASS TYPEID '{' feature_list '}' ';'  // example : class A {...};
    { $$ = class_($2,idtable.add_string("Object"),$4,
    stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';' // example : class A inherits B {...};
    { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    // If there is an error in a class definition but the class is terminated properly and the next class is
    // syntactically correct, the parser should be able to restart at the next class definition.
    | error ';'
    {}
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    feature_list:		/* empty */
    { $$ = nil_Features(); }
    | feature_list feature      // one more features
    { $$ = append_Features($1,single_Features($2)); }
    ;

    feature : OBJECTID '(' formal_list ')' ':' TYPEID '{' expr '}' ';'
    { $$ = method($1,$3,$6,$8); }      // example : function(...) : int { ... } 
    | OBJECTID ':' TYPEID ';'
    { $$ = attr($1,$3,no_expr());}     // example : a : int
    | OBJECTID ':' TYPEID ASSIGN expr ';'
    { $$ = attr($1,$3,$5); }           // example : a : int <- 1
    // recover from errors in features (going on to the next feature)
    | error '{' expr '}' ';'
    {}
    | error ';'
    {}
    ;
    
    formal_list : 
    { $$ = nil_Formals(); }         // empty formals
    | formal
    { $$ = single_Formals($1); }    // example : a : int
    | formal_list ',' formal
    { $$ = append_Formals($1,single_Formals($3)); }   // example : a : int , ...
    ;

    formal : OBJECTID ':' TYPEID
    { $$ = formal($1,$3); }        // example : a : int
    ;

    expr : OBJECTID ASSIGN expr  
    { $$ = assign($1,$3); }              // example : a <- 1
    | expr '.' OBJECTID '(' dispatch_exprs ')'
    { $$ = dispatch($1,$3,$5); }          // example : a.f(...)
    | expr '@' TYPEID '.' OBJECTID '(' dispatch_exprs ')'
    { $$ = static_dispatch($1,$3,$5,$7); }  // example : a@B.f(...), a should be a subclass instance of B (checked in semantic stage)
    | OBJECTID '(' dispatch_exprs ')'
    { $$ = dispatch(object(idtable.add_string("self")),$1,$3); }      // example : f(...)
    | IF expr THEN expr ELSE expr FI 
    { $$ = cond($2,$4,$6); }       // example : if a > 1 then a <- 1 else a <- 2 fi
    | WHILE expr LOOP expr POOL 
    { $$ = loop($2,$4); }       // example : while a < 10 LOOP a <- a + 1 POOL
    | '{' block_exprs '}'
    { $$ = block($2); }         // example : { ... }
    | LET let_expr              
    { $$ = $2; }                // example : let ...
    | CASE expr OF case_list ESAC
    { $$ = typcase($2,$4); }    // example : case a of ... esac
    | NEW TYPEID
    { $$ = new_($2); }          // example : new A
    | ISVOID expr 
    { $$ = isvoid($2); }        // example : isvoid(a)
    | expr '+' expr 
    { SET_NODELOC(@2);$$ = plus($1,$3); }       // example : 1 + 2
    | expr '-' expr 
    { SET_NODELOC(@2);$$ = sub($1,$3); }        // example : 2 - 1
    | expr '*' expr 
    { SET_NODELOC(@2);$$ = mul($1,$3); }        // example : 1 * 2
    | expr '/' expr 
    { SET_NODELOC(@2);$$ = divide($1,$3); }     // example : 4 / 2
    | '~' expr      
    { $$ = neg($2); }           // example : ~2
    | expr '<' expr  
    { SET_NODELOC(@2);$$ = lt($1,$3); }         // example : 1 < 2
    | expr LE expr 
    { SET_NODELOC(@2);$$ = leq($1,$3); }        // example : 1 <= 2
    | expr '=' expr 
    { SET_NODELOC(@2);$$ = eq($1,$3); }         // example : 2 = 2
    | NOT expr  
    { $$ = comp($2); }          // example : not a
    | '(' expr ')' 
    { $$ = $2; }                // example : (a)
    | OBJECTID    
    { $$ = object($1); }        // example : a
    | INT_CONST   
    { $$ = int_const($1); }     // example : 1
    | STR_CONST  
    { $$ = string_const($1); }  // example : "hello"
    | BOOL_CONST    
    { $$ = bool_const($1); }    // example : true
    ;

    dispatch_exprs : 
    { $$ = nil_Expressions(); }           // empty
    | expr
    { $$ = single_Expressions($1); }      // end expr
    | dispatch_exprs ',' expr
    { $$ = append_Expressions($1,single_Expressions($3)); }   // new expr
    ;

    block_exprs : expr ';'
    { $$ = single_Expressions($1); }      // end expr
    | block_exprs expr ';'
    { $$ = append_Expressions($1,single_Expressions($2)); }   // new expr
    | error ';'
    {}
    ;

    let_expr : OBJECTID ':' TYPEID ASSIGN expr IN expr
    { $$ = let($1,$3,$5,$7); }         // example : a : int <- 1 in ...
    | OBJECTID ':' TYPEID IN expr
    { $$ = let($1,$3,no_expr(),$5); }  // example : a : int in ...
    | OBJECTID ':' TYPEID ',' let_expr
    { $$ = let($1,$3,no_expr(),$5); }  // example : a : int , ...
    | OBJECTID ':' TYPEID ASSIGN expr ',' let_expr
    { $$ = let($1,$3,$5,$7); }         // example : a : int <- 1 , ...
    // a let binding (going on to the next variable)
    | error ',' let_expr
    { $$ = $3; }
    | error IN expr
    {}
    ;

    case_list : case
    { $$ = single_Cases($1); }            // end case
    | case_list case
    { $$ = append_Cases($1,single_Cases($2)); }         // new case
    ;

    case : OBJECTID ':' TYPEID DARROW expr ';'
    { $$ = branch($1,$3,$5); }      // example : a : int => a <- a + 1
    ;

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    