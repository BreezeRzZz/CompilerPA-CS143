#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>

enum Basicness
{
   Basic,
   NotBasic
};
#define TRUE 1
#define FALSE 0


class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
private:
   List<CgenNode> *nds;
   ostream &str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

public:
   // below are edited
   // used to handle self and SELF_TYPE
   CgenNodeP curr_class;
   // class name -> its attrs
   std::map<Symbol, std::vector<attr_class *>> attrTable;
   // class name -> its methods
   std::map<Symbol, std::vector<method_class *>> methodTable;
   // class name -> the offset of its attrs
   std::map<Symbol, std::map<Symbol, int>> attrOffsetTable;
   // class name -> the offset of its methods
   std::map<Symbol, std::map<Symbol, int>> methodOffsetTable;
   // class name -> class name(the method's), method name
   std::map<Symbol, std::vector<std::pair<Symbol, Symbol>>> dispatchTables;
   // class tag -> class
   std::map<int, CgenNodeP> tag_to_CgenNode;
   // for the class of this tag, how many children does it have?
   // needed in typcase code emit
   std::vector<int>  tag_children_num;
   // edit ends

   // The following methods emit code for
   // constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   // edited
   void install_attrs_and_methods();
   void install_classtags();
   void dfs(CgenNodeP node, int &tag);

   // need to be implemented
   // phase 1
   void code_class_nameTab();
   void code_class_objTab();
   void code_object_dispatchTabs();

   // phase 2
   void code_proto_objects();
   void code_object_inits();

   // phase 3
   void code_methods();

public:
   CgenClassTable(Classes, ostream &str);
   void code();
   CgenNodeP root();
};

class CgenNode : public class__class
{
private:
   int class_tag;
   CgenNodeP parentnd;       // Parent of class
   List<CgenNode> *children; // Children of class
   Basicness basic_status;   // `Basic' if class is basic
                             // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   // edited
   Features get_features() { return features; }
   std::vector<CgenNodeP> getInheritancePath();
   void set_classtag(int tag) { class_tag = tag; }
   int get_classtag() { return class_tag; }
};

class BoolConst
{
private:
   int val;

public:
   BoolConst(int);
   void code_def(ostream &, int boolclasstag);
   void code_ref(ostream &) const;
};
