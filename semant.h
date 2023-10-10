#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
// edited
#include <map>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
private:
  int semant_errors;
  void install_basic_classes();
  ostream &error_stream;

public:
  Class_ curr_class;
  std::map<Symbol, Class_> classTable;
  std::map<Class_, std::vector<method_class *>> methodTable;
  std::map<Class_, std::vector<attr_class *>> attrTable;
  SymbolTable<Symbol, Symbol> objectEnvironment;

  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  // edited
  ostream &semant_error(Expression e);
  ostream &semant_error(Feature f);
  ostream &semant_error(Case c);
  // end
  ostream &semant_error(Symbol filename, tree_node *t);

  // edited
  void check_main();
  std::vector<Class_> getInheritancePath(Class_ cls);
  std::vector<Class_> getInheritancePath(Symbol s);
  void check_inheritance();
  bool isAncestor(Class_ A, Class_ B);
  bool isAncestor(Symbol A, Symbol B);
  Symbol getLCA(Class_ A, Class_ B);
  Symbol getLCA(Symbol A, Symbol B);

  void install_attrs_and_methods();
  void check_attrs_and_methods();
  void check_attr(attr_class *attr);
  void check_method(method_class *method);
  void check_formals(method_class *method, Formals formals);
  bool check_override(method_class *method, const std::vector<Class_> &path);
  // end
};

#endif
