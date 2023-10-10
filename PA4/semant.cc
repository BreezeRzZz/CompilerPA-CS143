

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
// edited
#include <algorithm>
#include <set>

extern int semant_debug;
extern char *curr_filename;

ClassTable *classtable;
bool ifStop = false;
bool mainExist = false;
bool methodExist = false;
bool methodFormal = false;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Expression e)
{
    return semant_error(curr_class->get_filename(), e);
}

ostream &ClassTable::semant_error(Feature f)
{
    return semant_error(curr_class->get_filename(), f);
}

ostream &ClassTable::semant_error(Case c)
{
    return semant_error(curr_class->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

void ClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object,
               No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
        class_(IO,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                  SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                  SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
        class_(Str,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           append_Features(
                               single_Features(attr(val, Int, no_expr())),
                               single_Features(attr(str_field, prim_slot, no_expr()))),
                           single_Features(method(length, nil_Formals(), Int, no_expr()))),
                       single_Features(method(concat,
                                              single_Formals(formal(arg, Str)),
                                              Str,
                                              no_expr()))),
                   single_Features(method(substr,
                                          append_Formals(single_Formals(formal(arg, Int)),
                                                         single_Formals(formal(arg2, Int))),
                                          Str,
                                          no_expr()))),
               filename);

    // add basic classes
    classTable[Object] = Object_class;
    classTable[IO] = IO_class;
    classTable[Int] = Int_class;
    classTable[Bool] = Bool_class;
    classTable[Str] = Str_class;
}

// install all classes
ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    install_basic_classes();

    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        curr_class = classes->nth(i);
        Symbol cname = curr_class->get_name();
        Symbol pname = curr_class->get_parentname();

        // 1. class name can't be Int/Str/Bool/IO/Object/SELF_TYPE
        if (cname == Object || cname == IO || cname == Int || cname == Str || cname == Bool || cname == SELF_TYPE)
        {
            semant_error(curr_class) << "Redefinition of basic class " << cname << ".\n";
            ifStop = true;
        }

        // 2. class name shouldn't be redefined
        else if (classTable.find(cname) != classTable.end())
        {
            semant_error(curr_class) << "Class " << cname << " was previously defined.\n";
            ifStop = true;
        }
        else
        {
            if (semant_debug)
                cerr << "installing class: " << cname << ".\n";
            classTable[cname] = curr_class;
        }
    }
}

// check inheritance,including:
// 1. parent classes
// 2. inheritance cycle
// sequence:reverse.Why?
void ClassTable::check_inheritance()
{
    // check every class's parent class
    for (auto it = classTable.rbegin(); it != classTable.rend(); ++it)
    {
        Symbol parent_name = it->second->get_parentname();
        // parnet class cannot be Int/Bool/Str/SELF_TYPE
        if (parent_name == Int || parent_name == Bool || parent_name == Str || parent_name == SELF_TYPE)
        {
            semant_error(it->second) << "Class " << it->first << " cannot inherit class " << parent_name << ".\n";
            ifStop = true;
        }
        // note that Object's parent class is no_class
        // parent class should be defined
        else if (parent_name != No_class && classTable.find(parent_name) == classTable.end())
        {
            semant_error(it->second) << "Class " << it->first << " inherits from an undefined class " << parent_name << ".\n";
            ifStop = true;
        }
    }

    if (ifStop)
        return;

    for (auto it = classTable.rbegin(); it != classTable.rend(); ++it)
    {
        curr_class = it->second;
        if (semant_debug)
            cerr << "checking inheritance: now checking Class " << it->first << ".\n";
        Symbol cname = it->first;

        // need no check
        if (cname == Object)
            continue;

        // maintain slow and fast for each class
        Symbol slow = it->first;
        Symbol fast = it->first;
        while (true)
        {
            slow = classTable[slow]->get_parentname();
            fast = classTable[fast]->get_parentname();
            if (fast == Object)
                break;
            fast = classTable[fast]->get_parentname();
            if (fast == Object)
                break;
            // there exists a circle
            if (slow == fast)
            {
                semant_error(curr_class) << "Class " << cname << ", or an ancestor of " << cname << ", is involved in an inheritance cycle.\n";
                ifStop = true; // need to stop
                break;
            }
        }
    }
}

// check if A is B's ancestor?(i.e. B is conformed to A )
bool ClassTable::isAncestor(Class_ A, Class_ B)
{
    if (semant_debug)
        cerr << "in isAncestor:Before getInheritancePath\n";
    std::vector<Class_> pathB = getInheritancePath(B);

    // getInheritancePath is wrong
    if (semant_debug)
        cerr << "in isAncestor:After getInheritancePath\n";
    for (auto it = pathB.begin(); it != pathB.end(); ++it)
    {
        if ((*it)->get_name() == A->get_name())
            return true;
    }
    return false;
}

bool ClassTable::isAncestor(Symbol A, Symbol B)
{
    if (semant_debug)
    {
        cerr << "isAncestor:class name " << A << "\n";
        cerr << "isAncestor:class name " << B << "\n";
    }
    // NOTICE: we should handle SELF_TYPE
    if (A == SELF_TYPE && B == SELF_TYPE)
        return true;
    else if (A == SELF_TYPE)
        return false;
    else if (B == SELF_TYPE)
        return isAncestor(classTable[A], curr_class);
    if (classTable.find(A) == classTable.end() || classTable.find(B) == classTable.end())
    {
        if (errors())
        {
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }

    return isAncestor(classTable[A], classTable[B]);
}

// return the Least Comman Ancestor(LCA) of A and B.
Symbol ClassTable::getLCA(Class_ A, Class_ B)
{
    std::vector<Class_> pathA = getInheritancePath(A);
    std::vector<Class_> pathB = getInheritancePath(B);

    Class_ LCA = classTable[Object];

    for (auto itA = pathA.rbegin(), itB = pathB.rbegin(); itA != pathA.rend() && itB != pathB.rend(); ++itA, ++itB)
    {
        if ((*itA)->get_name() == (*itB)->get_name())
            LCA = *itA;
    }

    return LCA->get_name();
}

Symbol ClassTable::getLCA(Symbol A, Symbol B)
{
    // handle SELF_TYPE
    if (A == SELF_TYPE && B == SELF_TYPE)
        return SELF_TYPE;
    else if (A == SELF_TYPE)
        return getLCA(curr_class, classTable[B]);
    else if (B == SELF_TYPE)
        return getLCA(classTable[A], curr_class);

    if (classTable.find(A) == classTable.end() || classTable.find(B) == classTable.end())
    {
        if (errors())
        {
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }

    return getLCA(classTable[A], classTable[B]);
}

// get a path from c to Object
std::vector<Class_> ClassTable::getInheritancePath(Class_ c)
{
    std::vector<Class_> path;

    Class_ cls = c;

    // what happend to Object type?
    // Oh no! Object class can't get his name?
    if (semant_debug)
        cerr << "in getInheritancePath: " << cls->get_name() << endl;
    // we find that the Object class is a nullptr! Why?

    while (true)
    {
        path.push_back(cls);
        if (cls->get_name() == Object)
            break;
        // here is the problem:we shouldn't modify the pointer!
        // how can we restore it?
        // we shall change the logic:if find object, just stop

        // no. It seems class is already changed some before
        cls = classTable[cls->get_parentname()];
    }

    return path;
}

std::vector<Class_> ClassTable::getInheritancePath(Symbol s)
{
    if (classTable.find(s) == classTable.end())
    {
        if (errors())
        {
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }

    return getInheritancePath(classTable[s]);
}

// get a method table which records the list of <class,method list>
// and a attr table which records the list of <class,attribute list>
// this is mainly for convenience, and also do some important check.
void ClassTable::install_attrs_and_methods()
{
    // check as the program sequence?
    std::map<Class_, bool> checked;
    for (auto it = classTable.begin(); it != classTable.end(); ++it)
    {
        if (it->first == Main)
            mainExist = true;
        if (checked[it->second])
            continue;
        curr_class = it->second;
        auto path = getInheritancePath(curr_class);

        // check all class on the path,from Object to curr_class
        // size_t problem :(
        for (int pi = path.size() - 1; pi >= 0; --pi)
        {
            if (checked[path[pi]])
                continue;
            // checked
            checked[path[pi]] = true;
            if (semant_debug)
                cerr << "installing: checking " << path[pi]->get_name() << " " << path.size() << ".\n";
            std::vector<method_class *> curr_methods;
            std::vector<attr_class *> curr_attrs;
            auto features = path[pi]->get_features();
            for (int i = features->first(); features->more(i); i = features->next(i))
            {
                auto feature = features->nth(i);
                if (feature->is_attr())
                {
                    auto attr = (attr_class *)feature;
                    // self check
                    if (attr->get_name() == self)
                    {
                        semant_error(attr) << "'self' cannot be the name of an attribute.\n";
                        continue;
                    }
                    // find in inherited class
                    bool repeat = false;
                    // now checking class's ancestors(start from Object)
                    for (int pj = path.size() - 1; pj > pi; --pj)
                    {
                        // this parent class's attrs(after checking.Error attrs should not be installed)
                        auto pattrs = attrTable[path[pj]];
                        for (auto ait = pattrs.begin(); ait != pattrs.end(); ++ait)
                        {
                            if ((*ait)->get_name() == attr->get_name())
                            {
                                semant_error(attr) << "Attribute " << attr->get_name() << " is an attribute of an inherited class.\n";
                                repeat = true;
                                break;
                            }
                        }
                        if (repeat)
                            break;
                    }
                    // find in curr class
                    if (!repeat)
                    {
                        for (auto ait = curr_attrs.begin(); ait != curr_attrs.end(); ++ait)
                        {
                            if ((*ait)->get_name() == attr->get_name())
                            {
                                semant_error(attr) << "Attribute " << attr->get_name() << " is multiply defined in class.\n";
                                repeat = true;
                                break;
                            }
                        }
                    }

                    if (!repeat)
                        curr_attrs.push_back(attr);
                }
                else
                {
                    auto method = (method_class *)feature;
                    // find in inherited class
                    bool repeat = false;
                    for (int pj = path.size() - 1; pj > pi; --pj)
                    {
                        auto pmethods = methodTable[path[pj]];
                        for (auto mit = pmethods.begin(); mit != pmethods.end(); ++mit)
                        {
                            // check sequence is no matter
                            if ((*mit)->get_name() == method->get_name())
                            {
                                if (!check_override(method, path))
                                    repeat = true;
                                break;
                            }
                        }
                        if (repeat)
                            break;
                    }
                    // find in curr class
                    if (!repeat)
                    {
                        for (auto mit = curr_methods.begin(); mit != curr_methods.end(); ++mit)
                        {
                            if ((*mit)->get_name() == method->get_name())
                            {
                                semant_error(method) << "Method " << method->get_name() << " is multiply defined.\n";
                                repeat = true;
                                break;
                            }
                        }
                    }

                    if (!repeat)
                        curr_methods.push_back(method);
                }
            }
            methodTable[path[pi]] = curr_methods;
            attrTable[path[pi]] = curr_attrs;
        }
    }
    // check main
    if (mainExist)
    {
        auto methods = methodTable[classTable[Main]];
        for (auto it = methods.begin(); it != methods.end(); ++it)
        {
            if ((*it)->get_name() == main_meth)
            {
                methodExist = true;
                auto main_method = *it;
                auto formals = main_method->get_formals();
                if (formals->len() == 0)
                    methodFormal = true;
                break;
            }
        }
    }

    if (!mainExist)
        semant_error() << "Class Main is not defined.\n";
    else if (!methodExist)
        semant_error(classTable[Main]) << "No 'main' method in class Main.\n";
    else if (!methodFormal)
        semant_error(classTable[Main]) << "'main' method in class Main should have no arguments.\n";
}

// note we need to get a inheritance path from curr_class to Object
// why? Because the attrs and methods in parent class also appears in child class
// How can we implement it? From the root to curr_class, we enter scope every class, and not exit!
void ClassTable::check_attrs_and_methods()
{
    // we check every class
    for (auto it = classTable.begin(); it != classTable.end(); ++it)
    {
        if (semant_debug)
            cerr << "check every class: " << it->first << "\n";
        curr_class = it->second;
        // enter current scope
        objectEnvironment.enterscope();
        // get the inheritance path
        auto curr_path = getInheritancePath(curr_class);

        // prepare scope:attrs
        for (auto pit = curr_path.rbegin(); pit != curr_path.rend(); ++pit)
        {
            auto attrs = attrTable[*pit];
            for (auto ait = attrs.begin(); ait != attrs.end(); ++ait)
            {
                auto attr = *ait;
                objectEnvironment.addid(attr->get_name(), new Symbol(attr->get_type_decl()));
            }
        }

        // check curr_class's features
        // notice we'd better check in sequence
        auto features = curr_class->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i))
        {
            auto feature = features->nth(i);
            if (feature->is_attr())
                check_attr((attr_class *)feature);
            else
                check_method((method_class *)feature);
        }

        // exit current class's scope
        objectEnvironment.exitscope();
    }
}

void ClassTable::check_attr(attr_class *attr)
{
    Symbol attr_name = attr->get_name();
    Symbol attr_type = attr->get_type_decl();
    bool flag = false;

    // handle SELF_TYPE
    // (this will not change the real attr type, just used to do checking)
    if (attr_type == SELF_TYPE)
        attr_type = curr_class->get_name();

    // 1. does type_decl exist?
    // we should notice type prim_slot. It is the value type of Int and Bool
    if (attr_type != prim_slot && classTable.find(attr_type) == classTable.end())
    {
        semant_error(attr) << "Class " << attr_type << " of attribute " << attr_name << " is undefined.\n";
        flag = true;
    }
    // 2. does init_type is conformed to type_decl?
    // note we must handle no_type! very important!
    Symbol attr_init = attr->get_init()->checkType();
    if (attr_init == SELF_TYPE)
        attr_init = curr_class->get_name();

    if (!flag && attr_init != No_type && !isAncestor(attr_type, attr_init))
        semant_error(attr) << "Inferred type " << attr_init << " of initialization of attribute " << attr_name
                           << " does not conform to declared type " << attr_type << ".\n";
}

void ClassTable::check_method(method_class *method)
{
    // note that we need to enter a new scope for a method
    objectEnvironment.enterscope();
    Symbol method_name = method->get_name();
    Symbol method_returnType = method->get_return_type();
    Formals method_formals = method->get_formals();

    // here is OK
    if (semant_debug)
        cerr << "now checking " << method_name << "\n";

    // 1. check the formals
    // note the sequence.We must check each formal first,to add them to the current scope,then check the expr type
    check_formals(method, method_formals);

    // 2. is return_type defined?
    // if undefined, we should return at once. Otherwise checking the expr type and the return type will cause problem
    // notice SELF_TYPE. we should not convert in method definition!
    bool flag = false;
    if (method_returnType != SELF_TYPE && classTable.find(method_returnType) == classTable.end())
    {
        semant_error(method) << "Undefined return type " << method_returnType << " in method " << method_name << ".\n";
        flag = true;
    }

    // after return_type check
    Symbol method_exprType = method->get_expr()->checkType();
    if (semant_debug)
        cerr << "checking method: the method " << method->get_name() << "'s type is " << method_exprType << ".\n";

    // 3. check expr_type
    if (method_exprType != SELF_TYPE && classTable.find(method_exprType) == classTable.end())
        flag = true;

    // 4. is expr_type is conformed to return_type?
    if (!flag && method_exprType != No_type && !isAncestor(method_returnType, method_exprType))
        semant_error(method) << "Inferred return type " << method_exprType << " of method " << method_name
                             << " does not conform to declared return type " << method_returnType << ".\n";
    // exit this scope
    objectEnvironment.exitscope();
}

// method is used to provide error information
void ClassTable::check_formals(method_class *method, Formals formals)
{
    std::set<Symbol> formal_set;
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        Formal curr_formal = formals->nth(i);

        // 1. is type "SELF_TYPE"
        if (curr_formal->get_type_decl() == SELF_TYPE)
            semant_error(method) << "Formal parameter " << curr_formal->get_name() << " cannot have type SELF_TYPE.\n";

        // 2. is formal declared type defined?
        // note we continue to check next step
        else if (classTable.find(curr_formal->get_type_decl()) == classTable.end())
            semant_error(method) << "Class " << curr_formal->get_type_decl() << " of formal parameter " << curr_formal->get_name() << " is undefined.\n";

        // 3. is name "self"?
        if (curr_formal->get_name() == self)
        {
            semant_error(method) << "'self' cannot be the name of a formal parameter.\n";
            continue;
        }

        // 4. is formal redefined?
        if (formal_set.count(curr_formal->get_name()))
        {
            semant_error(method) << "Formal parameter " << curr_formal->get_name() << " is multiply defined.\n";
            continue;
        }
        else
            formal_set.insert(curr_formal->get_name());
        // and we need to add formal to current scope
        objectEnvironment.addid(curr_formal->get_name(), new Symbol(curr_formal->get_type_decl()));
    }
}

bool ClassTable::check_override(method_class *method, const std::vector<Class_> &path)
{
    // from curr_class's parent to object,check if exists this method
    // note we only need to check the nearest occurence, because we will check every method.(of course start from Object methods)
    for (auto it = path.begin() + 1; it != path.end(); ++it)
    {
        if (semant_debug)
            cerr << "in check_override: checking Class " << (*it)->get_name() << ".\n";
        // parent methods
        auto methods = methodTable[*it];
        // check if exist same method
        for (auto mit = methods.begin(); mit != methods.end(); ++mit)
        {
            if (semant_debug)
                cerr << "in Class " << (*it)->get_name() << ":checking method " << (*mit)->get_name() << ".\n";
            method_class *parent_method = *mit;
            if (parent_method->get_name() == method->get_name())
            {
                // check:
                // 1. is return type the same?
                if (parent_method->get_return_type() != method->get_return_type())
                {
                    semant_error(method) << "In redefined method " << method->get_name() << ", return type " << method->get_return_type()
                                         << " is different from original return type " << parent_method->get_return_type() << ".\n";
                    return false;
                }

                // get the formals
                Formals parent_formals = parent_method->get_formals();
                Formals formals = method->get_formals();

                // 2. is formal num the same?
                if (parent_formals->len() != formals->len())
                {
                    semant_error(method) << "Incompatible number of formal parameters in redefined method " << method->get_name() << ".\n";
                    return false;
                }

                // 3. is formal type the same? (sequence must be the same)
                for (int i = formals->first(); formals->more(i); i = formals->next(i))
                {
                    auto formal = formals->nth(i);
                    auto parent_formal = parent_formals->nth(i);

                    if (formal->get_type_decl() != parent_formal->get_type_decl())
                    {
                        semant_error(method) << "In redefined method " << method->get_name() << ", parameter type " << formal->get_type_decl()
                                             << " is different from original type " << parent_formal->get_type_decl() << "\n";
                        // why doesn't this error end with a period :)?
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

// ---------------------------------------------------------
// below are type checking area for expressions
// ---------------------------------------------------------

// example: a <- 1 + 1
// what is important is that we need to get the type of a(i.e. LHS).
Symbol assign_class::checkType()
{
    auto temp_type = classtable->objectEnvironment.lookup(name);
    Symbol id_type;
    // check LHS
    if (temp_type == NULL)
    {
        classtable->semant_error(this) << "Assignment to undeclared variable " << name << ".\n";
        id_type = Object;
    }
    else
    {
        if (name == self)
            classtable->semant_error(this) << "Cannot assign to 'self'.\n";
        id_type = *temp_type;
    }

    // check RHS
    Symbol e_type = expr->checkType();

    if (!classtable->isAncestor(id_type, e_type))
        classtable->semant_error(this) << "Type " << e_type << " of assigned expression does not conform to declared type "
                                       << id_type << " of identifier " << name << ".\n";

    // type is RHS
    type = e_type;

    return type;
}

// example: e0@A.f(e1,e2,e3)
Symbol static_dispatch_class::checkType()
{
    bool ifFind = false;
    Symbol e_type = expr->checkType();
    std::vector<Symbol> actual_types;
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        actual_types.push_back(actual->nth(i)->checkType());

    // check if type_name is defined
    if (classtable->classTable.find(type_name) == classtable->classTable.end())
    {
        classtable->semant_error(this) << "Static dispatch to undefined class " << type_name << ".\n";
        type = Object;
        return type;
    }

    // check if e_type is conformed to type_name
    if (!classtable->isAncestor(type_name, e_type))
    {
        classtable->semant_error(this) << "Expression type " << e_type
                                       << " does not conform to declared static dispatch type " << type_name << ".\n";
        type = Object;
        return type;
    }

    // get methods
    // we should find the method f
    auto path = classtable->getInheritancePath(e_type);
    for (auto pit = path.begin(); pit != path.end(); ++pit)
    {
        auto methods = classtable->methodTable[*pit];
        // find corresponding method
        for (auto mit = methods.begin(); mit != methods.end(); ++mit)
        {
            auto curr_method = *mit;
            if (curr_method->get_name() == name)
            {
                ifFind = true;

                // check the formals
                Formals method_formals = curr_method->get_formals();

                // check if actual num is the same as formal num
                if (method_formals->len() != actual->len())
                    classtable->semant_error(this) << "Method " << name << " invoked with wrong number of arguments.\n";

                // check each formal
                else
                    for (int i = method_formals->first(); method_formals->more(i); i = method_formals->next(i))
                    {
                        Formal curr_formal = method_formals->nth(i);
                        Symbol formal_type = curr_formal->get_type_decl();

                        // check if actual type is conformed to formal_type
                        Symbol actual_type = actual_types[i];
                        if (!classtable->isAncestor(formal_type, actual_type))

                            classtable->semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << curr_formal->get_name()
                                                           << " does not conform to declared type " << formal_type << ".\n";
                    }

                type = curr_method->get_return_type();
                break;
            }
        }
        if(ifFind)
            break;
    }

    if (ifFind)
    {
        if (type == SELF_TYPE)
            type = e_type;
        return type;
    }
    else
    {
        classtable->semant_error(this) << "Static dispatch to undefined method " << name << ".\n";
        type = Object;
        return type;
    }
}

// example: e0.f(e1,e2,e3)
Symbol dispatch_class::checkType()
{
    bool ifFind = false;
    Symbol e_type = expr->checkType();
    Symbol e_type_0 = e_type;
    std::vector<Symbol> actual_types;
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        actual_types.push_back(actual->nth(i)->checkType());
    // convert SELF_TYPE
    if (e_type == SELF_TYPE)
        e_type = classtable->curr_class->get_name();

    // we should find the method f
    auto path = classtable->getInheritancePath(e_type);
    for (auto pit = path.begin(); pit != path.end(); ++pit)
    {
        auto methods = classtable->methodTable[*pit];
        for (auto mit = methods.begin(); mit != methods.end(); ++mit)
        {
            auto curr_method = *mit;
            // find(only the first)
            if (curr_method->get_name() == name)
            {
                ifFind = true;

                // check the formals
                Formals method_formals = curr_method->get_formals();

                // check if actual num is the same as formal num
                if (method_formals->len() != actual->len())
                    classtable->semant_error(this) << "Method " << name << " called with wrong number of arguments.\n";

                // check each formal
                else
                    for (int i = method_formals->first(); method_formals->more(i); i = method_formals->next(i))
                    {
                        Formal curr_formal = method_formals->nth(i);
                        Symbol formal_type = curr_formal->get_type_decl();

                        // check if actual type is conformed to formal_type
                        Symbol actual_type = actual_types[i];
                        if (!classtable->isAncestor(formal_type, actual_type))
                            classtable->semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << curr_formal->get_name()
                                                           << " does not conform to declared type " << formal_type << ".\n";
                    }

                type = curr_method->get_return_type();
                break;
            }
        }
        if (ifFind)
            break;
    }

    if (ifFind)
    {
        if (type == SELF_TYPE)
            type = e_type_0; // should return original type
        return type;
    }
    else
    {
        classtable->semant_error(this) << "Dispatch to undefined method " << name << ".\n";
        type = Object;
        return type;
    }
}

Symbol cond_class::checkType()
{
    Symbol pred_type = pred->checkType();

    if (pred_type != Bool)
        classtable->semant_error(this) << "Predicate of 'if' does not have type Bool.\n";

    Symbol then_type = then_exp->checkType();
    Symbol else_type = else_exp->checkType();

    type = classtable->getLCA(then_type, else_type);
    return type;
}

Symbol loop_class::checkType()
{
    Symbol pred_type = pred->checkType();

    if (pred_type != Bool)
        classtable->semant_error(this) << "Loop condition does not have type Bool.\n";

    Symbol body_type = body->checkType();

    type = Object;
    return type;
}

// handle signle branch
// important: enter new scope,and check expr type
Symbol branch_class::checkType()
{
    // note we need to enter a new scope each branch
    classtable->objectEnvironment.enterscope();
    // Do we need to check type_decl is defined?
    if (name == self)
        classtable->semant_error(this) << "'self' bound in 'case'.\n";
    classtable->objectEnvironment.addid(name, new Symbol(type_decl));
    // we should check type in this scope
    Symbol type = expr->checkType();
    classtable->objectEnvironment.exitscope();
    return type;
}

// important: each case must have distinct type!
// example: case e0 of x1:T1 => ...;x2:T2 => ...;
Symbol typcase_class::checkType()
{
    expr->checkType();

    // use a set to check if duplicate
    std::set<Symbol> case_types;

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        branch_class *branch = (branch_class *)cases->nth(i);

        // check if duplicate(note we need to check type,not name)
        if (case_types.count(branch->get_type_decl()))
            // notice here to report branch's line number
            classtable->semant_error(branch) << "Duplicate branch " << branch->get_type_decl() << " in case statement.\n";
        else
            case_types.insert(branch->get_type_decl());

        // we find that branch_type is computed afterward
        Symbol branch_type = branch->checkType();

        // even the branch is duplicate, its branch type should still be gathered
        if (i > 0)
            type = classtable->getLCA(type, branch_type);
        else
            type = branch_type;
    }

    return type;
}

Symbol block_class::checkType()
{
    for (int i = body->first(); body->more(i); i = body->next(i))
        type = body->nth(i)->checkType();
    return type;
}

// example: let x: T0 <- e1 in e2
// or : let x:T0 in e1
Symbol let_class::checkType()
{
    Symbol init_type = init->checkType();
    bool flag = true;

    // check if type_decl is defined
    // notice SELF_TYPE
    if (type_decl != SELF_TYPE && classtable->classTable.find(type_decl) == classtable->classTable.end())
    {
        classtable->semant_error(this) << "Class " << type_decl
                                       << " of let-bound identifier " << identifier << " is undefined.\n";
        flag = false;
    }

    // check if conformed
    if (flag && init_type != No_type)
    {
        if (!classtable->isAncestor(type_decl, init_type))
            classtable->semant_error(this) << "Inferred type " << init_type << " of initialization of "
                                           << identifier << " does not conform to identifier's declared type " << type_decl << ".\n";
    }

    // enter a new scope
    classtable->objectEnvironment.enterscope();
    // identifier type is type_decl
    if (identifier == self)
        classtable->semant_error(this) << "'self' cannot be bound in a 'let' expression.\n";
    else
        classtable->objectEnvironment.addid(identifier, new Symbol(type_decl));
    // check body_type in current scope
    Symbol body_type = body->checkType();
    classtable->objectEnvironment.exitscope();

    type = body_type;

    return type;
}

Symbol plus_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " + "
                                       << e2_type << "\n";

    type = Int;
    return type;
}

Symbol sub_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " - "
                                       << e2_type << "\n";

    type = Int;
    return type;
}

Symbol mul_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " * "
                                       << e2_type << "\n";

    type = Int;
    return type;
}

Symbol divide_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " / "
                                       << e2_type << "\n";

    type = Int;
    return type;
}

Symbol neg_class::checkType()
{
    Symbol e1_type = e1->checkType();

    if (e1_type != Int)
        classtable->semant_error(this) << "Argument of '~' has type " << e1_type
                                       << " instead of Int.\n";

    type = Int;
    return type;
}

Symbol lt_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " < "
                                       << e2_type << "\n";

    type = Bool;
    return type;
}

Symbol eq_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type == Int || e1_type == Str || e1_type == Bool || e2_type == Int || e2_type == Str || e2_type == Bool)
    {
        if (e1_type != e2_type)
            classtable->semant_error(this) << "Illegal comparison with a basic type.\n";
    }

    type = Bool;
    return type;
}

Symbol leq_class::checkType()
{
    Symbol e1_type = e1->checkType();
    Symbol e2_type = e2->checkType();

    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(this) << "non-Int arguments: " << e1_type << " <= "
                                       << e2_type << "\n";

    type = Bool;
    return type;
}

Symbol comp_class::checkType()
{
    Symbol e1_type = e1->checkType();

    if (e1_type != Bool)
        classtable->semant_error(this) << "Argument of 'not' has type "
                                       << e1_type << " instead of Bool.\n";

    type = Bool;
    return type;
}

Symbol int_const_class::checkType()
{
    type = Int;
    return type;
}

Symbol bool_const_class::checkType()
{
    type = Bool;
    return type;
}

Symbol string_const_class::checkType()
{
    type = Str;
    return type;
}

Symbol new__class::checkType()
{
    // check whether the type is already declared
    if (type_name != SELF_TYPE && classtable->classTable.find(type_name) == classtable->classTable.end())
    {
        classtable->semant_error(this) << "'new' used with undefined class " << type_name << ".\n";
        type = Object;
        return type;
    }
    type = type_name;
    return type;
}

Symbol isvoid_class::checkType()
{
    e1->checkType();
    type = Bool;
    return type;
}

Symbol no_expr_class::checkType()
{
    type = No_type;
    return type;
}

Symbol object_class::checkType()
{
    // lookup from scope list
    auto temp_type = classtable->objectEnvironment.lookup(name);
    if (temp_type == NULL)
    {
        classtable->semant_error(this) << "Undeclared identifier " << name << ".\n";
        type = Object;
    }
    else
        type = *temp_type;

    return type;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);
    if (semant_debug)
        cerr << "after creating ClassTable\n";
    // install self
    classtable->objectEnvironment.enterscope();
    classtable->objectEnvironment.addid(self, new Symbol(SELF_TYPE));

    /* some semantic analysis code may go here */
    // check inheritance first
    classtable->check_inheritance();

    if (!ifStop)
    {
        classtable->install_attrs_and_methods();
        if (semant_debug)
            cerr << "after install methods and attrs\n";

        classtable->check_attrs_and_methods();
        if (semant_debug)
            cerr << "after checking all attrs and methods\n";
    }

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}