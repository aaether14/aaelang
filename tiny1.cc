#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "gimple-expr.h"
#include "diagnostic.h"
#include "opts.h"
#include "fold-const.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "debug.h"
#include "convert.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "common/common-target.h"
#include "tiny/tiny-parser.h"



struct GTY(()) lang_type
{
	char dummy;
};



struct GTY(()) lang_decl
{
	char dummy;
};



struct GTY(()) lang_identifier
{
	struct tree_identifier common;
};



union GTY((desc("TREE_CODE(&%h.generic) == IDENTIFIER_NODE"),
	    chain_next("CODE_CONTAINS_STRUCT(TREE_CODE(&%h.generic), "
			"TS_COMMON) ? ((union lang_tree_node *)TREE_CHAIN "
			"(&%h.generic)) : NULL"))) lang_tree_node
{
	union tree_node GTY((tag("0"), desc("tree_node_structure(&%h)")))generic;
	struct lang_identifier GTY((tag ("1")))identifier;
};



struct GTY(()) language_function
{
	char dummy;
};


static bool tiny_langhook_init(void)
{

	build_common_tree_nodes(false);
	void_list_node = build_tree_list(NULL_TREE, void_type_node);
	build_common_builtin_nodes();
	return true;
}



static void tiny_langhook_parse_file(void)
{
	tiny_parse_files(num_in_fnames, in_fnames);
}



static tree tiny_langhook_type_for_mode(enum machine_mode mode, int unsignedp)
{

	
	if (mode == TYPE_MODE(float_type_node))
		return float_type_node;

	if (mode == TYPE_MODE(double_type_node))
		return double_type_node;


	if (mode == TYPE_MODE(intQI_type_node))
		return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
	if (mode == TYPE_MODE(intHI_type_node))
		return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
	if (mode == TYPE_MODE(intSI_type_node))
		return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
	if (mode == TYPE_MODE(intDI_type_node))
		return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
	if (mode == TYPE_MODE(intTI_type_node))
		return unsignedp ? unsigned_intTI_type_node : intTI_type_node;


  	if (mode == TYPE_MODE(integer_type_node))
		return unsignedp ? unsigned_type_node : integer_type_node;


	if (mode == TYPE_MODE(long_integer_type_node))
		return unsignedp ? long_unsigned_type_node : long_integer_type_node;


	if (mode == TYPE_MODE(long_long_integer_type_node))
		return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;


  	if (COMPLEX_MODE_P(mode))
	{
		if (mode == TYPE_MODE(complex_float_type_node))
			return complex_float_type_node;
		if (mode == TYPE_MODE(complex_double_type_node))
			return complex_double_type_node;
		if (mode == TYPE_MODE(complex_long_double_type_node))
			return complex_long_double_type_node;
		if (mode == TYPE_MODE(complex_integer_type_node) && !unsignedp)
			return complex_integer_type_node;
	}
	return NULL;

}


static tree tiny_langhook_type_for_size(unsigned int bits, int unsignedp)
{

	tree type;
	if (unsignedp)
	{
		if (bits == INT_TYPE_SIZE)
			type = unsigned_type_node;
		else if (bits == CHAR_TYPE_SIZE)
			type = unsigned_char_type_node;
		else if (bits == SHORT_TYPE_SIZE)
			type = short_unsigned_type_node;
		else if (bits == LONG_TYPE_SIZE)
			type = long_unsigned_type_node;
		else if (bits == LONG_LONG_TYPE_SIZE)
			type = long_long_unsigned_type_node;
		else
			type = make_unsigned_type(bits);
	}
	else
	{
		if (bits == INT_TYPE_SIZE)
			type = integer_type_node;
		else if (bits == CHAR_TYPE_SIZE)
			type = signed_char_type_node;
		else if (bits == SHORT_TYPE_SIZE)
			type = short_integer_type_node;
		else if (bits == LONG_TYPE_SIZE)
			type = long_integer_type_node;
		else if (bits == LONG_LONG_TYPE_SIZE)
			type = long_long_integer_type_node;
		else
			type = make_signed_type(bits);
	}
	return type;
}


static tree tiny_langhook_builtin_function(tree decl)
{
	return decl;
}



static bool tiny_langhook_global_bindings_p(void)
{
	return false;
}


static tree tiny_langhook_pushdecl(tree decl ATTRIBUTE_UNUSED)
{
	gcc_unreachable();
}


static tree tiny_langhook_getdecls(void)
{
	return NULL;
}


#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "Tiny"

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT tiny_langhook_init

#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE tiny_langhook_parse_file

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE tiny_langhook_type_for_mode

#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE tiny_langhook_type_for_size

#undef LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION tiny_langhook_builtin_function

#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#define LANG_HOOKS_GLOBAL_BINDINGS_P tiny_langhook_global_bindings_p

#undef LANG_HOOKS_PUSHDECL
#define LANG_HOOKS_PUSHDECL tiny_langhook_pushdecl

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS tiny_langhook_getdecls

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-tiny-tiny1.h"
#include "gtype-tiny.h"
