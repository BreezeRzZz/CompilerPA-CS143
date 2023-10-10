
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <algorithm>
#include <string>

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

// edited
static int label = 0;
static CgenClassTableP codegen_classtable;
static int maxLocalNum = 0;
static int currLocalNum = 0;
static SymbolTable<Symbol, int> objectEnvironment;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char *gc_init_names[] =
	{"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
	{"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
	// spim wants comments to start with '#'
	os << "# start of generated code\n";

	initialize_constants();
	codegen_classtable = new CgenClassTable(classes, os);
	codegen_classtable->code();
	codegen_classtable->exitscope();

	os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s)
{
	s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
	  << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s)
{
	s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
	  << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s)
{
	s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s)
{
	s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s)
{
	s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s)
{
	emit_partial_load_address(dest, s);
	b.code_ref(s);
	s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s)
{
	emit_partial_load_address(dest, s);
	str->code_ref(s);
	s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s)
{
	emit_partial_load_address(dest, s);
	i->code_ref(s);
	s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s)
{
	s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s)
{
	s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s)
{
	s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s)
{
	s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s)
{
	s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s)
{
	s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s)
{
	s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s)
{
	s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s)
{
	s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s)
{
	s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s)
{
	s << JAL << address << endl;
}

static void emit_return(ostream &s)
{
	s << RET << endl;
}

static void emit_gc_assign(ostream &s)
{
	s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
	s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
	s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s)
{
	s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
	s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
	s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s)
{
	emit_label_ref(l, s);
	s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
	s << BEQZ << source << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
	s << BEQ << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
	s << BNE << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
	s << BLEQ << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
	s << BLT << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
	s << BLT << src1 << " " << imm << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
	s << BGT << src1 << " " << imm << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_branch(int l, ostream &s)
{
	s << BRANCH;
	emit_label_ref(l, s);
	s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str)
{
	emit_store(reg, 0, SP, str);
	emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s)
{
	emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s)
{
	emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
	emit_push(ACC, s);
	emit_move(ACC, SP, s);	// stack end
	emit_move(A1, ZERO, s); // allocate nothing
	s << JAL << gc_collect_names[cgen_Memmgr] << endl;
	emit_addiu(SP, SP, 4, s);
	emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s)
{
	if (source != (char *)A1)
		emit_move(A1, source, s);
	s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s)
{
	s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag)
{
	IntEntryP lensym = inttable.add_int(len);

	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL																// label
	  << WORD << stringclasstag << endl										// tag
	  << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
	  << WORD;

	/***** Add dispatch information for class String ******/
	// we just need to declare.
	emit_disptable_ref(Str, s);

	s << endl; // dispatch table
	s << WORD;
	lensym->code_ref(s);
	s << endl;					  // string length
	emit_string_constant(s, str); // ascii string
	s << ALIGN;					  // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag)
{
	for (List<StringEntry> *l = tbl; l; l = l->tl())
		l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
	s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL											 // label
	  << WORD << intclasstag << endl					 // class tag
	  << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
	  << WORD;

	/***** Add dispatch information for class Int ******/
	emit_disptable_ref(Int, s);

	s << endl;				  // dispatch table
	s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
	for (List<IntEntry> *l = tbl; l; l = l->tl())
		l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
	s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag)
{
	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL											  // label
	  << WORD << boolclasstag << endl					  // class tag
	  << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
	  << WORD;

	/***** Add dispatch information for class Bool ******/
	emit_disptable_ref(Bool, s);

	s << endl;				  // dispatch table
	s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
	Symbol main = idtable.lookup_string(MAINNAME);
	Symbol string = idtable.lookup_string(STRINGNAME);
	Symbol integer = idtable.lookup_string(INTNAME);
	Symbol boolc = idtable.lookup_string(BOOLNAME);

	str << "\t.data\n"
		<< ALIGN;
	//
	// The following global names must be defined first.
	//
	str << GLOBAL << CLASSNAMETAB << endl;
	str << GLOBAL;
	emit_protobj_ref(main, str);
	str << endl;
	str << GLOBAL;
	emit_protobj_ref(integer, str);
	str << endl;
	str << GLOBAL;
	emit_protobj_ref(string, str);
	str << endl;
	str << GLOBAL;
	falsebool.code_ref(str);
	str << endl;
	str << GLOBAL;
	truebool.code_ref(str);
	str << endl;
	str << GLOBAL << INTTAG << endl;
	str << GLOBAL << BOOLTAG << endl;
	str << GLOBAL << STRINGTAG << endl;

	//
	// We also need to know the tag of the Int, String, and Bool classes
	// during code generation.
	//
	str << INTTAG << LABEL
		<< WORD << intclasstag << endl;
	str << BOOLTAG << LABEL
		<< WORD << boolclasstag << endl;
	str << STRINGTAG << LABEL
		<< WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
	str << GLOBAL << HEAP_START << endl
		<< HEAP_START << LABEL
		<< WORD << 0 << endl
		<< "\t.text" << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Main"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Int"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("String"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Bool"), str);
	str << endl
		<< GLOBAL;
	emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
	str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
	falsebool.code_def(str, boolclasstag);
	truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
	//
	// Generate GC choice constants (pointers to GC functions)
	//
	str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
	str << "_MemMgr_INITIALIZER:" << endl;
	str << WORD << gc_init_names[cgen_Memmgr] << endl;
	str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
	str << "_MemMgr_COLLECTOR:" << endl;
	str << WORD << gc_collect_names[cgen_Memmgr] << endl;
	str << GLOBAL << "_MemMgr_TEST" << endl;
	str << "_MemMgr_TEST:" << endl;
	str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
	//
	// Add constants that are required by the code generator.
	//
	stringtable.add_string("");
	inttable.add_string("0");

	stringtable.code_string_table(str, stringclasstag);
	inttable.code_string_table(str, intclasstag);
	code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
	stringclasstag = 0 /* Change to your String class tag here */;
	intclasstag = 0 /* Change to your Int class tag here */;
	boolclasstag = 0 /* Change to your Bool class tag here */;

	enterscope();
	if (cgen_debug)
		cout << "Building CgenClassTable" << endl;
	install_basic_classes();
	install_classes(classes);
	build_inheritance_tree();
	// now we have a class graph
	install_classtags();
	install_attrs_and_methods();
}

void CgenClassTable::install_basic_classes()
{

	// The tree package uses these globals to annotate the classes built below.
	// curr_lineno  = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	//
	// A few special class names are installed in the lookup table but not
	// the class list.  Thus, these classes exist, but are not part of the
	// inheritance hierarchy.
	// No_class serves as the parent of Object and the other special classes.
	// SELF_TYPE is the self class; it cannot be redefined or inherited.
	// prim_slot is a class known to the code generator.
	//
	addid(No_class,
		  new CgenNode(class_(No_class, No_class, nil_Features(), filename),
					   Basic, this));
	addid(SELF_TYPE,
		  new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
					   Basic, this));
	addid(prim_slot,
		  new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
					   Basic, this));

	//
	// The Object class has no parent class. Its methods are
	//        cool_abort() : Object    aborts the program
	//        type_name() : Str        returns a string representation of class name
	//        copy() : SELF_TYPE       returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.
	//
	install_class(
		new CgenNode(
			class_(Object,
				   No_class,
				   append_Features(
					   append_Features(
						   single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
						   single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
					   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
				   filename),
			Basic, this));

	//
	// The IO class inherits from Object. Its methods are
	//        out_string(Str) : SELF_TYPE          writes a string to the output
	//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
	//        in_string() : Str                    reads a string from the input
	//        in_int() : Int                         "   an int     "  "     "
	//
	install_class(
		new CgenNode(
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
				   filename),
			Basic, this));

	//
	// The Int class has no methods and only a single attribute, the
	// "val" for the integer.
	//
	install_class(
		new CgenNode(
			class_(Int,
				   Object,
				   single_Features(attr(val, prim_slot, no_expr())),
				   filename),
			Basic, this));

	//
	// Bool also has only the "val" slot.
	//
	install_class(
		new CgenNode(
			class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
			Basic, this));

	//
	// The class Str has a number of slots and operations:
	//       val                                  ???
	//       str_field                            the string itself
	//       length() : Int                       length of the string
	//       concat(arg: Str) : Str               string concatenation
	//       substr(arg: Int, arg2: Int): Str     substring
	//
	install_class(
		new CgenNode(
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
				   filename),
			Basic, this));
}

int get_label()
{
	return label++;
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
	{
		return;
	}

	// The class name is legal, so add it to the list of classes
	// and the symbol table.
	nds = new List<CgenNode>(nd, nds);
	addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
	for (int i = cs->first(); cs->more(i); i = cs->next(i))
		install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
	for (List<CgenNode> *l = nds; l; l = l->tl())
		set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
	CgenNode *parent_node = probe(nd->get_parent());
	nd->set_parentnd(parent_node);
	parent_node->add_child(nd);
}

// construct the method table and attr table
// and the offsetMethodTable, as well as the dispatchTable
void CgenClassTable::install_attrs_and_methods()
{
	CgenNodeP node;
	Features features;
	// traverse the class list
	for (auto nodelist = nds; nodelist; nodelist = nodelist->tl())
	{
		node = nodelist->hd();
		features = node->get_features();

		// traverse this class's features
		for (int i = features->first(); features->more(i); i = features->next(i))
		{
			auto feature = features->nth(i);
			if (feature->is_method())
				methodTable[node->get_name()].push_back((method_class *)feature);
			else
				attrTable[node->get_name()].push_back((attr_class *)feature);
		}
	}

	// after installing the methodTable and attrTable
	// now check the whole path for each class(CgenNode)
	for (auto nodelist = nds; nodelist; nodelist = nodelist->tl())
	{
		node = nodelist->hd();
		auto cur_classname = node->get_name();
		auto path = node->getInheritancePath();

		// path is from Object to this class
		// why this sequence? it is the convention of dispatchTable:
		// from top to bottom, the sequence is from Object to current class
		for (auto it = path.rbegin(); it != path.rend(); ++it)
		{
			auto class_name = (*it)->get_name();
			auto methods = methodTable[(*it)->get_name()];
			for (auto method : methods)
			{
				// now we need to generate the offset table and dispatchTable of this class
				// what does dispatchTable mean? it means we need to
				// find the closet method apperance in this path!
				// the offset means where the method should be put
				// why we need this? Suppose that the method we dispatch is overrided
				// and these overrided methods may not be in the same position in different classes.
				// so with such a offsetTable, if we have a method name, we can directly get its position in each class(if exist)
				// This is the requirement of MIPS instruction to find the appropriate offset.(Otherwise we may use a find function...)

				// if not find, i.e. the first occurence of ancestor class
				// add to offsetTable, and the dispatchTable:
				// we need to dispatch this method if the dispatcher is this class
				if (methodOffsetTable[cur_classname].find(method->name) == methodOffsetTable[cur_classname].end())
				{
					dispatchTables[cur_classname].push_back({class_name, method->name});
					methodOffsetTable[cur_classname][method->name] = dispatchTables[cur_classname].size() - 1;
				}
				// if find, need to override
				// change cur class's this offset method into a newer version
				else
				{
					auto offset = methodOffsetTable[cur_classname][method->name];
					// override
					dispatchTables[cur_classname][offset] = {class_name, method->name};
				}
			}
		}
	}
}

// assign a unique classtag for each class
// notice the classtag sequence follows dfs(preorder) sequence
void CgenClassTable::install_classtags()
{
	int tag = 0;
	CgenNodeP node = lookup(Object);
	dfs(node, tag);
}

int get_children_num(CgenNodeP node)
{
	int num = 0;
	for (auto child = node->get_children(); child; child = child->tl())
		num += 1 + get_children_num(child->hd());
	return num;
}

void CgenClassTable::dfs(CgenNodeP node, int &tag)
{
	// assign current tag to this node
	node->set_classtag(tag);
	tag_to_CgenNode[tag] = node;

	// need to handle String/Int/Bool
	Symbol name = node->get_name();
	if (name == Str)
		stringclasstag = tag;
	else if (name == Bool)
		boolclasstag = tag;
	else if (name == Int)
		intclasstag = tag;
	tag += 1;
	// dfs for all children
	for (auto child = node->get_children(); child; child = child->tl())
	{
		if (cgen_debug)
			cerr << node->name << "'s child: " << child->hd()->name << endl;
		dfs(child->hd(), tag);
	}
}

std::vector<CgenNodeP> CgenNode::getInheritancePath()
{
	// recursively get its parent,until no_class
	// there will be no semantic error, of course!
	std::vector<CgenNodeP> path;
	auto node = this;
	while (true)
	{
		path.push_back(node);
		if (node->get_name() == Object)
			break;
		node = node->get_parentnd();
	}
	return path;
}

void CgenNode::add_child(CgenNodeP n)
{
	children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
	assert(parentnd == NULL);
	assert(p != NULL);
	parentnd = p;
}

void CgenClassTable::code()
{
	if (cgen_debug)
		cout << "coding global data" << endl;
	code_global_data();

	if (cgen_debug)
		cout << "choosing gc" << endl;
	code_select_gc();

	if (cgen_debug)
		cout << "coding constants" << endl;
	code_constants();

	//                 Add your code to emit
	//                   - prototype objects
	//                   - class_nameTab
	//                   - dispatch tables
	//
	code_class_nameTab();
	code_class_objTab();
	code_object_dispatchTabs();

	code_proto_objects();

	if (cgen_debug)
		cout << "coding global text" << endl;
	code_global_text();

	//                 Add your code to emit
	//                   - object initializer
	//                   - the class methods
	//                   - etc...
	code_object_inits();
	code_methods();
}

CgenNodeP CgenClassTable::root()
{
	return probe(Object);
}

// below are edited

// construct the class name table
// Description:
// A table, which at index (class tag) * 4 contains a pointer
// to a String object containing the name of the class associated
void CgenClassTable::code_class_nameTab()
{
	// we just need to traverse the class then emit code
	// notice the sequence: the tag from 0 to max.
	// i.e. the preorder of the AST
	// we can use the tag_to_CgenNodeP, to avoid actual traversal

	// here we first declare the class name tab LABEL
	str << CLASSNAMETAB << LABEL;

	for (int tag = 0; tag < int(tag_to_CgenNode.size()); ++tag)
	{
		// the nameTab should be like:
		// class_nameTab:
		// 		.word	str_const1
		// 		.word	str_const2
		// 		...
		Symbol name = tag_to_CgenNode[tag]->get_name();
		auto str_entry = stringtable.lookup_string(name->get_string());
		str << WORD;
		str_entry->code_ref(str);
		str << endl;
	}
}

// construct the class object table
// Description:
// A table, which at index (class tag) * 8 contains a pointer to
// the prototype object and at index (class tag) * 8 + 4 contains
// a pointer to the initialization method for that class.
void CgenClassTable::code_class_objTab()
{
	// the same construction as nameTab.
	str << CLASSOBJTAB << LABEL;

	for (int tag = 0; tag < int(tag_to_CgenNode.size()); ++tag)
	{
		// the objTab should be like:
		// class_objTab:
		//		.word	Object_protObj
		//		.word	Object_init
		//		...
		Symbol name = tag_to_CgenNode[tag]->get_name();
		str << WORD;
		emit_protobj_ref(name, str);
		str << endl;

		str << WORD;
		emit_init_ref(name, str);
		str << endl;
	}
}

// Description:
// The dynamic method dispatch bond table
// i.e. when an Object of some class is dispatching some method,
// which method should actually be used?
// after installing dispatchTable, we just need to emit corrsponding code
void CgenClassTable::code_object_dispatchTabs()
{
	// the dispatchTable for class A should be like:
	// A_dispatchtab:
	// Object.abort()
	// ...
	// A.func()
	// ...
	for (auto it = dispatchTables.begin(); it != dispatchTables.end(); ++it)
	{
		auto cur_name = it->first;
		auto dispatchTable = it->second;
		emit_disptable_ref(cur_name, str);
		str << LABEL;
		for (auto dit = dispatchTable.begin(); dit != dispatchTable.end(); ++dit)
		{
			str << WORD;
			auto class_name = dit->first;
			auto method_name = dit->second;
			emit_method_ref(class_name, method_name, str);
			str << endl;
		}
	}
}

// Description:
// The prototype Object foe each class
// it is used when we need to initialize a new object of one class
// at this time we will dispatch Object.copy() method, to copy prototype object
// then the class's init method
void CgenClassTable::code_proto_objects()
{
	// layout of a prototype:
	// -4 Garbage Collector Tag
	// 0 Class Tag
	// 4 Object Size(in word）
	// 8 Dispatch Table
	// >=12 Attributes（from ancestor's to current class's
	for (auto nodelist = nds; nodelist; nodelist = nodelist->tl())
	{
		auto node = nodelist->hd();
		auto cur_name = node->get_name();
		// this is Garbage Collector Tag.
		str << WORD << -1 << endl;
		// should be like: Object_protObj
		emit_protobj_ref(cur_name, str);
		str << LABEL;
		// this is Class Tag
		str << WORD << node->get_classtag() << endl;
		// now the Object size(in word),which is equally asking how many attrs we have
		int objSize = DEFAULT_OBJFIELDS; // class tag, object size and dispatch table. They totally measure 3 words(12 bytes)
		// traverse the attrs, including inherited attrs!
		auto path = node->getInheritancePath();
		for (auto it = path.begin(); it != path.end(); ++it)
		{
			auto class_name = (*it)->get_name();
			auto attrs = attrTable[class_name];
			objSize += attrs.size();
		}

		// output the object size
		str << WORD << objSize << endl;

		// next is the dispatch pointer
		// only need to generate a reference
		str << WORD;
		emit_disptable_ref(cur_name, str);
		str << endl;

		// the rest is the attrs. From Object to current class!
		// attr start from 12 (in word size is 3)
		int attr_pos = DEFAULT_OBJFIELDS;
		for (auto it = path.rbegin(); it != path.rend(); ++it)
		{
			auto class_name = (*it)->get_name();
			auto attrs = attrTable[class_name];
			for (auto attr : attrs)
			{
				// By the way we can install the attrOffsetTable
				// it is used to look for an attr's position(offset) in a class
				attrOffsetTable[cur_name][attr->name] = attr_pos;
				++attr_pos;
				str << WORD;
				// we need to set the default value of the attr
				// especially notice Int/Bool/String

				// String: default ""
				if (attr->type_decl == Str)
				{
					auto string_entry = stringtable.lookup_string("");
					string_entry->code_ref(str);
				}
				// Bool: default false
				else if (attr->type_decl == Bool)
					falsebool.code_ref(str);
				// Int: default 0
				else if (attr->type_decl == Int)
				{
					auto int_entry = inttable.lookup_string("0");
					int_entry->code_ref(str);
				}
				// other types(may be modified...)
				else
					str << 0;
				str << endl;
			}
		}
	}
}

// change stack pointer and framepointer
// store old fp,$s0,$ra.
void begin_method(int local_num, ostream &s)
{
	int total_bytes = (local_num + 3) * WORD_SIZE;
	// 12 bytes for: old fp,$s0,$ra
	emit_addiu(SP, SP, -total_bytes, s);
	emit_store(FP, local_num + 3, SP, s);	// store old fp
	emit_store(SELF, local_num + 2, SP, s); // store $s0
	emit_store(RA, local_num + 1, SP, s);	// store $ra

	// set current $fp
	emit_addiu(FP, SP, WORD_SIZE, s);
	// save ACC to SELF
	emit_move(SELF, ACC, s);
}

void end_method(int formal_num, int local_num, ostream &s)
{
	int total_bytes = (formal_num + local_num + 3) * WORD_SIZE;
	emit_load(FP, local_num + 3, SP, s);   // restore $fp
	emit_load(SELF, local_num + 2, SP, s); // restore $s0
	emit_load(RA, local_num + 1, SP, s);   // restore $ra

	// restore $sp
	emit_addiu(SP, SP, total_bytes, s);
	emit_return(s); // equals to: jr $ra
}

// this is used to init the attributes of a class(including inherited attrs)
// note we may need to dispatch ancestor's init method
void CgenClassTable::code_object_inits()
{
	// check each class
	for (auto nodelist = nds; nodelist; nodelist = nodelist->tl())
	{
		curr_class = nodelist->hd();
		emit_init_ref(curr_class->get_name(), str);
		str << LABEL;

		maxLocalNum = 0;
		if (!curr_class->basic())
		{
			auto attrs = attrTable[curr_class->get_name()];
			// first pass: determine the stack layout of init method
			for (auto attr : attrs)
			{
				if (attr->init->get_type())
					attr->init->check();
			}
		}

		// prepare for method begin
		// init method has no formals
		begin_method(maxLocalNum, str);
		// handle parent class
		// diaspatch parent's init method
		// notice this should be recursive.So only dispatch once here
		auto parent_class = curr_class->get_parentnd();
		if (parent_class->get_name() != No_class)
		{
			str << JAL;
			emit_init_ref(parent_class->get_name(), str);
			str << endl;
		}

		// handle current class
		// This is the real part of handling attr init
		if (!curr_class->basic())
		{
			auto attrs = attrTable[curr_class->get_name()];
			for (auto attr : attrs)
			{
				// when no init, do not need to store into $s0
				// notice: No_expression's type is NULL,not No_type
				if (attr->init->get_type())
				{
					attr->init->code(str); // expr object in $a0
					int attr_offset = attrOffsetTable[curr_class->get_name()][attr->name];
					emit_store(ACC, attr_offset, SELF, str); // find the attr,store into $s0
				}
			}
		}
		emit_move(ACC, SELF, str);
		end_method(0, maxLocalNum, str);
	}
}

// generate code for method(i.e. the method expr)
// here we need to store the formal and its offset(in reverse order)
void CgenClassTable::code_methods()
{
	for (auto nodelist = nds; nodelist; nodelist = nodelist->tl())
	{
		curr_class = nodelist->hd();
		if (curr_class->basic())
			continue;

		// only handle current class's methods
		auto methods = methodTable[curr_class->get_name()];
		for (auto method : methods)
		{
			// get the max local num to clarify the layout of method stack
			maxLocalNum = 0;
			// check the method to determine the stack layout
			method->expr->check();
			emit_method_ref(curr_class->get_name(), method->name, str);
			str << LABEL;
			auto formals = method->formals;
			begin_method(maxLocalNum, str);
			// put formal into stack in reverse orders
			// don't need to emit code for formals
			int formal_offset = maxLocalNum + 3;
			objectEnvironment.enterscope();
			for (int i = formals->len() - 1; i >= 0; --i)
			{
				auto formal = formals->nth(i);
				objectEnvironment.addid(formal->get_name(), new int(formal_offset));
				++formal_offset;
			}

			// method body code
			method->expr->code(str);
			objectEnvironment.exitscope();
			end_method(formals->len(), maxLocalNum, str);
		}
	}
}
// end edited

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) : class__class((const class__class &)*nd),
																	   parentnd(NULL),
																	   children(NULL),
																	   basic_status(bstatus)
{
	// we may not add No_class,prim_slot and SELF_TYPE
	if (name == SELF_TYPE || name == prim_slot || name == No_class)
		return;
	stringtable.add_string(name->get_string()); // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s)
{
	// expr code
	// result in $a0
	expr->code(s);
	// find the identifier
	// (cannot assign self)
	if (objectEnvironment.lookup(name) != NULL)
	{
		// for formal, it is above $fp, the offset is positive
		// for local variable, it is below $fp, the offset is negative
		// anyway we just need to add the offset and $fp to get the right val
		int offset = *(objectEnvironment.lookup(name));
		emit_store(ACC, offset, FP, s);
		return;
	}

	// attribute
	int offset = codegen_classtable->attrOffsetTable[codegen_classtable->curr_class->get_name()][name];
	emit_store(ACC, offset, SELF, s);
}

void static_dispatch_class::code(ostream &s)
{
	// first the formals, in order
	for (int i = actual->first(); actual->more(i); i = actual->next(i))
	{
		auto cur_actual = actual->nth(i);
		cur_actual->code(s); // $a0 stores this actual object
		// push into stack
		emit_push(ACC, s);
	}
	// next LHS expr
	expr->code(s); // $a0 stores the dispatcher object
	// we should test if the dispatcher is NULL
	int nonvoid_label = get_label();
	emit_bne(ACC, ZERO, nonvoid_label, s);
	// this section is for dispatch on void
	// need to prepare runtime error information
	// see cool-runtime

	// store filename in $a0
	emit_load_string(ACC, stringtable.lookup_string(codegen_classtable->curr_class->filename->get_string()), s);
	// this is the line number
	emit_load_imm(T1, this->line_number, s);
	// immediately abort
	emit_jal("_dispatch_abort", s);

	// this section is for normal dispatch
	// for static dispatch, only slight difference
	// that is, to search in given class's methodOffsetTable
	emit_label_def(nonvoid_label, s);

	// notice how to find the given class's dispatchTable. it is different from dispatch
	// because you cannot know the exact type of dispatch expr before running
	// so dispatch has to fetch dispatchTable addr from expr Object
	// but static dispatch is different
	std::string static_dispatchTable = type_name->get_string() + std::string(DISPTAB_SUFFIX);
	emit_load_address(T1, (char *)static_dispatchTable.c_str(), s);
	
	// now find the corresponding method in dispatch table (of given type)
	int offset = codegen_classtable->methodOffsetTable[type_name][name];
	emit_load(T1, offset, T1, s); // now $t1 stores the pointer to the method we need
	emit_jalr(T1, s);			  // dispatch the method
}

void dispatch_class::code(ostream &s)
{
	// first the formals, in order
	for (int i = actual->first(); actual->more(i); i = actual->next(i))
	{
		auto cur_actual = actual->nth(i);
		cur_actual->code(s); // $a0 stores this actual object
		// push into stack
		emit_push(ACC, s);
	}
	// next LHS expr
	expr->code(s); // $a0 stores the dispatcher object
	// we should test if the dispatcher is NULL
	int nonvoid_label = get_label();
	emit_bne(ACC, ZERO, nonvoid_label, s);
	// this section is for dispatch on void
	// need to prepare runtime error information
	// see cool-runtime

	// store filename in $a0
	emit_load_string(ACC, stringtable.lookup_string(codegen_classtable->curr_class->filename->get_string()), s);
	// this is the line number
	emit_load_imm(T1, this->line_number, s);
	// immediately abort
	emit_jal("_dispatch_abort", s);

	// this section is for normal dispatch
	emit_label_def(nonvoid_label, s);
	emit_load(T1, DISPTABLE_OFFSET, ACC, s); // $t1 now points to this object's dispatch table
	Symbol expr_type = expr->type;			 // this is static type
	if (expr_type == SELF_TYPE)
		expr_type = codegen_classtable->curr_class->get_name();
	// now find the corresponding method in dispatch table
	int offset = codegen_classtable->methodOffsetTable[expr_type][name];
	emit_load(T1, offset, T1, s); // now $t1 stores the pointer to the method we need
	emit_jalr(T1, s);			  // dispatch the method
}

void cond_class::code(ostream &s)
{
	pred->code(s);
	emit_fetch_int(T1, ACC, s); // $t1 now stores the bool val

	int false_label = get_label();
	int end_label = get_label();
	emit_beqz(T1, false_label, s);
	// the section is for true
	then_exp->code(s);
	emit_branch(end_label, s);
	// the section is for false
	emit_label_def(false_label, s);
	else_exp->code(s);

	// end
	emit_label_def(end_label, s);
}

void loop_class::code(ostream &s)
{
	int test_label = get_label();
	int out_label = get_label();

	// test the condition
	emit_label_def(test_label, s);
	pred->code(s);
	emit_fetch_int(T1, ACC, s); // get the boolean value
	emit_beqz(T1, out_label, s);
	// if satisfy the condition
	// do the body code
	body->code(s);
	// back to condition test
	emit_branch(test_label, s);

	// this is the out
	emit_label_def(out_label, s);
	// notice: we need to make $a0 void
	emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s)
{
	expr->code(s);
	// check if type is void
	int end_label = get_label();
	int nonvoid_label = get_label();
	emit_bne(ACC, ZERO, nonvoid_label, s);
	// this section is for void handles
	emit_load_string(ACC, stringtable.lookup_string(codegen_classtable->curr_class->get_filename()->get_string()), s);
	emit_load_imm(T1, this->get_line_number(), s);
	emit_jal("_case_abort2", s);

	// this section is for nonvoid

	emit_label_def(nonvoid_label, s);
	// now expr object in $a0
	// first we should store it in stack
	emit_store(ACC, currLocalNum, FP, s);
	// fetch its class tag
	emit_load(T2, 0, ACC, s);

	// first sort the branches by class tag
	// then one by one check
	std::vector<std::pair<branch_class *, int>> temp;
	for (int i = cases->first(); cases->more(i); i = cases->next(i))
	{
		auto branch = (branch_class *)cases->nth(i);
		// get classtag
		int tag = codegen_classtable->lookup(branch->type_decl)->get_classtag();
		temp.push_back({branch, tag});
	}

	// sort by classtag
	// from large to small
	sort(temp.begin(), temp.end(), [](const std::pair<branch_class *, int> A, const std::pair<branch_class *, int> B)
		 { return A.second > B.second; });

	int next_branch_label;
	// now check each branch
	for (auto it = temp.begin(); it != temp.end(); ++it)
	{
		auto branch = it->first;
		auto tag = it->second;
		if (cgen_debug)
			cerr << branch->name << ' ' << tag << endl;

		int childrenNum = get_children_num(codegen_classtable->tag_to_CgenNode[tag]);

		if (it != temp.begin())
			emit_label_def(next_branch_label, s);

		next_branch_label = get_label();
		// if Class A is Class B's subclass
		// then: suppose class tag of A is a, class tag of B is b
		// A <= B iff b <= a <= b + k, where k is childrenNum of b
		emit_blti(T2, tag, next_branch_label, s);
		emit_bgti(T2, tag + childrenNum, next_branch_label, s);
		// this section is for execting this branch
		objectEnvironment.enterscope();
		objectEnvironment.addid(branch->name, new int(currLocalNum));
		++currLocalNum;
		branch->expr->code(s);
		--currLocalNum;
		objectEnvironment.exitscope();
		// after choosing one branch,immediately jump to end
		emit_branch(end_label, s);
	}

	// this is for no match error
	emit_label_def(next_branch_label, s);
	emit_jal("_case_abort", s);

	// end
	emit_label_def(end_label, s);
}

void block_class::code(ostream &s)
{
	for (int i = body->first(); body->more(i); i = body->next(i))
	{
		auto expr = body->nth(i);
		expr->code(s);
	}
	// finally $a0 stores last expression's result
}

void let_class::code(ostream &s)
{
	// if there is no init
	if (init->get_type() == NULL)
	{
		// specially handle Int/Bool/String
		// assign their default value
		if (type_decl == Int)
			emit_load_int(ACC, inttable.lookup_string("0"), s);
		else if (type_decl == Str)
			emit_load_string(ACC, stringtable.lookup_string(""), s);
		else if (type_decl == Bool)
			emit_load_bool(ACC, falsebool, s);
		else
			init->code(s);
	}
	else
		init->code(s); // init object in $a0

	objectEnvironment.enterscope();
	objectEnvironment.addid(identifier, new int(currLocalNum));
	// official implemetation use callee saved registers
	// for our implemetation, it is to be improved

	// save init to corresponding position
	emit_store(ACC, currLocalNum, FP, s);
	++currLocalNum;
	body->code(s);
	--currLocalNum;
	objectEnvironment.exitscope();
}

void plus_class::code(ostream &s)
{
	e1->code(s);
	// save result in stack
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_jal("Object.copy", s);		 // copy current $a0(i.e. e2) in $a0 as a new Int object
	emit_load(T1, e1_offset, FP, s); // load previous result from stacks
	emit_fetch_int(T1, T1, s);		 // $t1 = M[$s1 + 12] (i.e. the value of Int object 1)
	emit_fetch_int(T2, ACC, s);		 // $t2 = M[$a0 + 12] (i.e. the value of Int object 2)
	emit_add(T1, T1, T2, s);		 // $t1 = $t1 + $t2
	emit_store_int(T1, ACC, s);		 // M[$a0 + 12] = $t1 (store the value)
	--currLocalNum;
}

void sub_class::code(ostream &s)
{
	e1->code(s);
	// save result in stack
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_jal("Object.copy", s);		 // copy current $a0(i.e. e2) in $a0 as a new Int object
	emit_load(T1, e1_offset, FP, s); // load previous result from stacks
	emit_fetch_int(T1, T1, s);		 // $t1 = M[$s1 + 12] (i.e. the value of Int object 1)
	emit_fetch_int(T2, ACC, s);		 // $t2 = M[$a0 + 12] (i.e. the value of Int object 2)
	emit_sub(T1, T1, T2, s);		 // $t1 = $t1 - $t2
	emit_store_int(T1, ACC, s);		 // M[$a0 + 12] = $t1 (store the value)
	--currLocalNum;
}

void mul_class::code(ostream &s)
{
	e1->code(s);
	// save result in stack
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_jal("Object.copy", s);		 // copy current $a0(i.e. e2) in $a0 as a new Int object
	emit_load(T1, e1_offset, FP, s); // load previous result from stacks
	emit_fetch_int(T1, T1, s);		 // $t1 = M[$s1 + 12] (i.e. the value of Int object 1)
	emit_fetch_int(T2, ACC, s);		 // $t2 = M[$a0 + 12] (i.e. the value of Int object 2)
	emit_mul(T1, T1, T2, s);		 // $t1 = $t1 * $t2
	emit_store_int(T1, ACC, s);		 // M[$a0 + 12] = $t1 (store the value)
	--currLocalNum;
}

// notice don't need to handle divide by zero
void divide_class::code(ostream &s)
{
	e1->code(s);
	// save result in stack
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_jal("Object.copy", s);		 // copy current $a0(i.e. e2) in $a0 as a new Int object
	emit_load(T1, e1_offset, FP, s); // load previous result from stacks
	emit_fetch_int(T1, T1, s);		 // $t1 = M[$s1 + 12] (i.e. the value of Int object 1)
	emit_fetch_int(T2, ACC, s);		 // $t2 = M[$a0 + 12] (i.e. the value of Int object 2)
	emit_div(T1, T1, T2, s);		 // $t1 = $t1 / $t2
	emit_store_int(T1, ACC, s);		 // M[$a0 + 12] = $t1 (store the value)
	--currLocalNum;
}

void neg_class::code(ostream &s)
{
	e1->code(s);				// e1 object in $a0
	emit_jal("Object.copy", s); // copy a new Int object of e1 in $a0
	emit_fetch_int(T1, ACC, s); // fetch value attr of Int
	emit_neg(T1, T1, s);		// $t1 = -$t1
	emit_store_int(T1, ACC, s); // store back to $a0
}

void lt_class::code(ostream &s)
{
	e1->code(s);
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_load(T1, e1_offset, FP, s); // $t1 = e1 Object
	emit_move(T2, ACC, s);			 // $t2 = e2 Object
	emit_fetch_int(T1, T1, s);		 // $t1 = e1.val
	emit_fetch_int(T2, T2, s);		 // $t2 = e2.val

	int end_label = get_label();
	emit_load_bool(ACC, truebool, s);
	emit_blt(T1, T2, end_label, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(end_label, s);
	--currLocalNum;
}

void eq_class::code(ostream &s)
{
	e1->code(s);
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_load(T1, e1_offset, FP, s);
	emit_move(T2, ACC, s);

	// compare the pointer first
	// if pointer the same, then must be the same
	int equal_label = get_label();
	emit_load_bool(ACC, truebool, s);
	emit_beq(T1, T2, equal_label, s);
	emit_load_bool(A1, falsebool, s);
	// else if pointer not the same, and the static type is the Int/Bool/String
	// then we can compare the val
	// here we dispatch function eqaulity_test. it is a builtin function
	// put true in $a0, false in $a1

	emit_jal("equality_test", s);
	emit_label_def(equal_label, s);

	--currLocalNum;
}

void leq_class::code(ostream &s)
{
	e1->code(s);
	emit_store(ACC, currLocalNum, FP, s);
	int e1_offset = currLocalNum;
	++currLocalNum;
	e2->code(s);
	emit_load(T1, e1_offset, FP, s); // $t1 = e1 Object
	emit_move(T2, ACC, s);			 // $t2 = e2 Object
	emit_fetch_int(T1, T1, s);		 // $t1 = e1.val
	emit_fetch_int(T2, T2, s);		 // $t2 = e2.val

	int end_label = get_label();
	emit_load_bool(ACC, truebool, s);
	emit_bleq(T1, T2, end_label, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(end_label, s);
	--currLocalNum;
}

void comp_class::code(ostream &s)
{
	e1->code(s);
	emit_fetch_int(T1, ACC, s); // It is OK to use fetch_int to get the first attr
	int false_label = get_label();
	emit_load_bool(ACC, truebool, s);
	emit_beqz(T1, false_label, s); // if e1.val == false
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(false_label, s);
}

void int_const_class::code(ostream &s)
{
	//
	// Need to be sure we have an IntEntry *, not an arbitrary Symbol
	//
	emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s)
{
	emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s)
{
	emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s)
{
	std::string object_name = type_name->get_string();
	if (type_name == SELF_TYPE)
	{
		// for SELF_TYPE, we have to find the appropriate class first
		// notice we need emit code to realize this

		// first, get the class tag of curr_class
		emit_load(T1, 0, SELF, s);
		// then get the class_objTab
		emit_load_address(T2, CLASSOBJTAB, s);

		// classtag * 8, this is the mapping from class tag to offset
		emit_sll(T1, T1, 3, s);
		// add the offset and the class_objTab addr, to get the real addr
		// now $t1 is addr of current class's protoObj
		emit_addu(T1, T1, T2, s);
		// get the content of this addr
		// $a0 = M[$T1]. i.e. the protoObj
		emit_load(ACC, 0, T1, s);
		// copy
		emit_jal("Object.copy", s);
		// now need to init this object
		// get the content of init_method
		// note that $t1 is likely to be changed during the copy
		// so we should save it, as callee-saved reg
		// or compute twice, this is a simple but low-effi method...

		emit_load(T1, 0, SELF, s);
		emit_load_address(T2, CLASSOBJTAB, s);
		emit_sll(T1, T1, 3, s);
		emit_addu(T1, T1, T2, s);
		emit_load(A1, 1, T1, s); // +4 offset for init_method
		emit_jalr(A1, s);
	}
	// we need to find the corresponding proto_obj, and copy it to $a0
	// how can we find it? we just need to load address...
	// then init the object. i.e. dispatch the init method
	else
	{
		auto protobj_addr = object_name + PROTOBJ_SUFFIX;
		emit_load_address(ACC, (char *)protobj_addr.c_str(), s);
		emit_jal("Object.copy", s);
		object_name = type_name->get_string();
		auto init_addr = object_name + CLASSINIT_SUFFIX;
		emit_jal((char *)init_addr.c_str(), s);
	}
}

void isvoid_class::code(ostream &s)
{
	e1->code(s);
	emit_move(T1, ACC, s);

	// notice we directly compare the pointer
	// i.e. is the pointer 0?
	int eq_zero_label = get_label();
	emit_load_bool(ACC, truebool, s);
	emit_beqz(T1, eq_zero_label, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(eq_zero_label, s);
}

// void
// namely, NULL pointer(ZERO doesn't mean int value 0!!)
void no_expr_class::code(ostream &s)
{
	emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s)
{
	if (name == self)
	{
		emit_move(ACC, SELF, s);
		return;
	}

	// search in objectEnvironment first
	if (objectEnvironment.lookup(name) != NULL)
	{
		// anyway we just need to add the offset and $fp to get the right val
		int offset = *(objectEnvironment.lookup(name));
		emit_load(ACC, offset, FP, s);
		return;
	}

	// then must be attribute
	// (no semantic error!)
	int offset = codegen_classtable->attrOffsetTable[codegen_classtable->curr_class->get_name()][name];
	emit_load(ACC, offset, SELF, s);
}

///////////////////////////////////////////////////////////////////////
//
// Expression traversal
// 		To determine the method stack layout
//
///////////////////////////////////////////////////////////////////////

void assign_class::check()
{
	expr->check();
}

void static_dispatch_class::check()
{
	expr->check();
	for (int i = actual->first(); actual->more(i); i = actual->next(i))
		actual->nth(i)->check();
}

void dispatch_class::check()
{
	expr->check();
	for (int i = actual->first(); actual->more(i); i = actual->next(i))
		actual->nth(i)->check();
}

void cond_class::check()
{
	pred->check();
	then_exp->check();
	else_exp->check();
}

void loop_class::check()
{
	pred->check();
	body->check();
}

void typcase_class::check()
{
	expr->check();
	for (int i = cases->first(); cases->more(i); i = cases->next(i))
	{
		auto branch = (branch_class *)cases->nth(i);
		// change local num
		++currLocalNum;
		maxLocalNum = std::max(maxLocalNum, currLocalNum);
		branch->expr->check();
		--currLocalNum;
	}
}

void block_class::check()
{
	for (int i = body->first(); body->more(i); i = body->next(i))
		body->nth(i)->check();
}

void let_class::check()
{
	// change local num
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	init->check();
	body->check();
	--currLocalNum;
}

void plus_class::check()
{
	// change local num
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void sub_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void mul_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void divide_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void neg_class::check()
{
	e1->check();
}

void lt_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void eq_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void leq_class::check()
{
	++currLocalNum;
	maxLocalNum = std::max(maxLocalNum, currLocalNum);
	e1->check();
	e2->check();
	--currLocalNum;
}

void comp_class::check()
{
	e1->check();
}

void int_const_class::check()
{
	return;
}

void string_const_class::check()
{
	return;
}

void bool_const_class::check()
{
	return;
}

void new__class::check()
{
	return;
}

void isvoid_class::check()
{
	e1->check();
}

void no_expr_class::check()
{
	return;
}

void object_class::check()
{
	return;
}