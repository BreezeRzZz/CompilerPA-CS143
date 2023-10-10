/*
 *  The scanner definition for COOL.
 */
%option noyywrap
/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
#define tooLong 1
#define nullCharacter 2
#define escapedNull 3
int nestDepth = 0;
int whatError = 0;
int errorLine = 1;
std::string buffer;
%}

/*
 * Define names for regular expressions here.
 */
%x ncomments
%x icomments
%x string
%x stringError
DIGIT           [0-9]
DIGITS          {DIGIT}+
OBJECT_ID       [a-z][a-zA-Z0-9_]*
TYPE_ID         [A-Z][a-zA-Z0-9_]*
WHITESPACE      [ \f\r\t\v]

%%

 /* Nested comments */
 /* Comments may also be written by enclosing text in (*...*). The latter form of comment may be nested.(cool-manual 10.3)*/
 /* We should notice how to handle "nested" feature.For example: (*aaa(*bbb*)*) */
 /* if we just simply jump out of the start condition once meet a "*)" first,then the latter "*)" should not be treated as a part of comments */
 /* However,I tried in a cool file,found that the latter "*)" is actually a part of comments */
 /* That is to say, we must keep track of the actual depth of nesting, in order to describe a nested comment concisely. */
<INITIAL,ncomments>"(*"                  {if(errorLine > curr_lineno) curr_lineno = errorLine; ++nestDepth;BEGIN(ncomments);}
<ncomments>"\n"                          {++curr_lineno;}
<ncomments>"*)"                          {--nestDepth;if(nestDepth == 0) BEGIN(INITIAL);}
 /* a comment remains open when EOF is encountered (PA2 4.2)*/
<ncomments><<EOF>>                       {yylval.error_msg = "EOF in comment";BEGIN(INITIAL);return (ERROR);}
<ncomments>.                             {}
 /* see "*)" outside a comment (PA2 4.2)*/
"*)"                                     {if(errorLine > curr_lineno) curr_lineno = errorLine;yylval.error_msg = "Unmatched *)";return (ERROR);}


 /* inline comments */
 /* Any characters between two dashes "--" and the next newline (or EOF, if there is no next newline) are treated as comments(cool manual 10.3)*/
"--"                 {if(errorLine > curr_lineno) curr_lineno = errorLine;BEGIN(icomments);}
 /* a comment remains open when EOF is encountered (PA2 4.2)*/
<icomments><<EOF>>   {yylval.error_msg = "EOF in comment";BEGIN(INITIAL);return (ERROR);}
<icomments>"\n"      {BEGIN(INITIAL);++curr_lineno;}
<icomments>.         {}


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  * (COOL manual 10.4)
  * Keywords have higher priority than Identifiers.So write keyword rules first.
  * See <Flex manual 7>:If it finds two or more matches of the same length, the rule listed first in the flex input file is chosen.
  * (I guess the order of tokens in cool-parse.h may indicate the priorities among them)
  */
(?i:class)       {if(errorLine > curr_lineno) curr_lineno = errorLine;return (CLASS);}
(?i:else)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (ELSE);}
(?i:fi)          {if(errorLine > curr_lineno) curr_lineno = errorLine;return (FI);}
(?i:if)          {if(errorLine > curr_lineno) curr_lineno = errorLine;return (IF);}
(?i:in)          {if(errorLine > curr_lineno) curr_lineno = errorLine;return (IN);}
(?i:inherits)    {if(errorLine > curr_lineno) curr_lineno = errorLine;return (INHERITS);}
(?i:isvoid)      {if(errorLine > curr_lineno) curr_lineno = errorLine;return (ISVOID);}
(?i:let)         {if(errorLine > curr_lineno) curr_lineno = errorLine;return (LET);}
(?i:loop)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (LOOP);}
(?i:pool)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (POOL);}
(?i:then)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (THEN);}
(?i:while)       {if(errorLine > curr_lineno) curr_lineno = errorLine;return (WHILE);}
(?i:case)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (CASE);}
(?i:esac)        {if(errorLine > curr_lineno) curr_lineno = errorLine;return (ESAC);}
(?i:new)         {if(errorLine > curr_lineno) curr_lineno = errorLine;return (NEW);}
(?i:of)          {if(errorLine > curr_lineno) curr_lineno = errorLine;return (OF);}
(?i:not)         {if(errorLine > curr_lineno) curr_lineno = errorLine;return (NOT);}


 /* For class IDs,object IDs,integers,and strings,the sematic value should */
 /* be a symbol stored in the field cool_yylval.symbol (PA2 5) */

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *  Strings need to be pay more attention.You cannot just simply record the yytext into the stringtable.
  *  Because once a token is matched, the yytext will be new.
  *  So we need a buffer to store the whole string.Otherwise,you can only see a STR_CONST "\"" in your output.
  */
"\""                  {if(errorLine > curr_lineno) curr_lineno = errorLine;BEGIN(string);buffer="";}
<string>"\""          {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  else{
    cool_yylval.symbol = stringtable.add_string((char*)buffer.c_str());
    BEGIN(INITIAL);
    return (STR_CONST);
  }
}

 /* If a string contains an unescaped newline, report that error as "Unterminated string constant" */
 /* and resume lexing at the beginning of the next line */
<string>"\n"      {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "Unterminated string constant";
  if(errorLine > curr_lineno) 
    curr_lineno = errorLine;
  ++curr_lineno;
  return (ERROR);
}
 /* If the string contains null character (PA2 4.1)*/
<string>"\\\0"    {whatError=escapedNull;errorLine = curr_lineno;BEGIN(stringError);}
<string>"\0"      {whatError=nullCharacter;errorLine = curr_lineno;BEGIN(stringError);}
<string><<EOF>>   {cool_yylval.error_msg = "EOF in string constant";BEGIN(INITIAL);return (ERROR);}
 /* Handle escape characters. */
 /* For special sequence, We need to make "\\." into "\."(its actual meaning),and add to buffer(so we can see the right value in STR_CONST)*/
 /* Note we don't need to increment current line number with "\\n" */
 /* For normal sequence,just return the following character */

<string>"\\n"        {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += "\n";
}
<string>"\\t"        {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += "\t";
}
<string>"\\b"        {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += "\b";
}
<string>"\\f"        {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += "\f";
}
<string>"\\\n"       {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += "\n";
  ++curr_lineno;
}
<string>\\.         {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += yytext[1];
}
<string>.         {
  if(buffer.length() >= MAX_STR_CONST)
  {
    whatError = tooLong;
    errorLine = curr_lineno;
    BEGIN(stringError);
  }
  buffer += yytext;
}

 /* String Error */
 /* when find a " or a unescaped newline,end */
 /* notice we need to restore the correct value to curr_lineno after returning */
<stringError>"\""|"\n"   {
  if(yytext[0]=='\n')
    ++errorLine;

  switch(whatError){
    case tooLong:
    cool_yylval.error_msg = "String constant too long";
    BEGIN(INITIAL);
    return (ERROR);
    case nullCharacter:
    cool_yylval.error_msg = "String contains null character.";
    BEGIN(INITIAL);
    return (ERROR);
    case escapedNull:
    cool_yylval.error_msg = "String contains escaped null character.";
    BEGIN(INITIAL);
    return (ERROR);
  }
}
 /* if meet an EOF when already error */
<stringError><<EOF>>  {
  switch(whatError){
    case tooLong:
    cool_yylval.error_msg = "String constant too long";
    BEGIN(INITIAL);
    return (ERROR);
    case nullCharacter:
    cool_yylval.error_msg = "String contains null character.";
    BEGIN(INITIAL);
    return (ERROR);
    case escapedNull:
    cool_yylval.error_msg = "String contains escaped null character.";
    BEGIN(INITIAL);
    return (ERROR);
  }
}
<stringError>"\\\n"     {++errorLine;}
<stringError>.          {}

 /* Integers. Integers are non-empty strings of digits 0-9.(cool-manual 10.1) */
 /* Here I write a simple COOL program to test if an integer can start with any number of 0s.*/
 /* The answer is yes, integer such as 001 can sucessfully be regarded as the correct value 1.*/
 /* So we don't need to do any special correction to the first digit of an integer */
{DIGITS}             {if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.symbol = inttable.add_string(yytext);return (INT_CONST);}

 /* true and false */
 /* For boolean constants,the semantic value is stored in the field cool_yylval.boolean(PA2 5) */
 /* in cool-parse.h, BOOLEAN is actually defined as type int */
 /* same as keyword, higher priority than identifer. */
t(?i:rue)        {if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.boolean = 1; return (BOOL_CONST);}
f(?i:alse)       {if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.boolean = 0; return (BOOL_CONST);}

 /* (Object) Identifiers. 
  * Identifiers are strings (other than keywords) consisting of letters, digits, and the underscore character
  * object identifiers begin with a lower case letter(cool-manual 10.1)
  */
{OBJECT_ID}          {if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.symbol = idtable.add_string(yytext);return (OBJECTID);}

 /* Type Identifiers. Type identifiers begin with a capital letter.(cool-manual 10.1) */
{TYPE_ID}            {if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.symbol = idtable.add_string(yytext);return (TYPEID);}
 
 /* newline */
"\n"                 {if(errorLine > curr_lineno) curr_lineno = errorLine;++curr_lineno;}
 /* whitespace (cool-manual 10.5)*/
{WHITESPACE}         {if(errorLine > curr_lineno) curr_lineno = errorLine;}

 /* The multiple-character operators. */
"=>"		             { if(errorLine > curr_lineno) curr_lineno = errorLine;return (DARROW); }
"<-"                 { if(errorLine > curr_lineno) curr_lineno = errorLine;return (ASSIGN); }
"<="                 { if(errorLine > curr_lineno) curr_lineno = errorLine;return     {LE}; }


 /* The single-character operators. */
 /* Cool has four binary arithmetic operations: +, -, *,/  */
 /* three comparison operations: <, <=(multiple-character operator), =. */
 /* one arithmetic and one logical unary operator:~ and not(keyword) (cool-manual 7.12) */
 /* The tokens for single character symbols are represented just by the integer(ASCII) value of the character itself (PA2 5) */
 /* The full list we can refer utilities.cc */
 /* notice that we should use single quote, because the language actually used is C */
"+"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('+'); }
"-"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('-'); }
"*"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('*'); }
"/"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('/'); }
"="                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('='); }
"<"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('<'); }
"."                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('.'); }
"~"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('~'); }
","                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int(','); }
";"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int(';'); }
":"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int(':'); }     
"("                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('('); }
")"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int(')'); }
"@"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('@'); }   
"{"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('{'); }
"}"                  { if(errorLine > curr_lineno) curr_lineno = errorLine;return int('}'); }

 /* Errors. */
 /* When an invalid character (one that can't begin any token) is encountered, */
 /* a string containing just that character should be returned as the error string. */
 /* (Here I'm a little confused.Does the "string" means the input string, or the string const? maybe the former one.)*/
 /* and to get the same result as sample, we should record the value into error_msg .*/
.                    { if(errorLine > curr_lineno) curr_lineno = errorLine;cool_yylval.error_msg = yytext;return (ERROR);} 
%%
