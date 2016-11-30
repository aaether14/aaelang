#include <iostream>
#include <algorithm>
#include <memory>



#include "tiny/tiny-parser.h"
#include "tiny/tiny-lexer.h"
#include "aae_scope_manager.h"
#include "aae_error_manager.h"



#include "target.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"



namespace AAELang
{


class Parser
{


	private:
	/** 
	*Panic mode recovery
	*/
	void SkipAfterToken(AAELang::TokenId);



	/**
	*Token handling 
	*/
	bool SkipToken(AAELang::TokenId);
	const_TokenPtr ExpectToken(AAELang::TokenId);
	void UnexpectedToken(const_TokenPtr);



	/**
	*Expression handling
	*/
	int left_binding_power(const_TokenPtr tok);
	Tree null_denotation(const_TokenPtr tok);
	Tree left_denotation(const_TokenPtr tok, Tree left);
	Tree parse_expression(int right_binding_power);
	Tree coerce_binary_arithmetic(const_TokenPtr tok, Tree *left, Tree *right);
	bool check_logical_operands(const_TokenPtr tok, Tree left, Tree right);



	Tree build_label_decl(const char *name, location_t loc);
	Tree build_if_statement(Tree bool_expr, Tree then_part, Tree else_part);
	Tree build_while_statement(Tree bool_expr, Tree while_body);
	Tree build_for_statement(SymbolPtr ind_var, Tree lower_bound, Tree upper_bound,
			    Tree for_body_stmt_list);



	const char *print_type(Tree type);



	void parse_statement_sequence(bool (Parser::*done)());
	void parse_statement_or_block();
	bool done_brace();
	bool done_end_of_file();


	/**
	*Statement parsing methods
	*/
	Tree parse_statement();
	Tree parse_variable_declaration();
	Tree parse_type_declaration();
	Tree parse_type();
	Tree parse_record();
	Tree parse_field_declaration(std::vector<std::string> &field_names);
	Tree parse_assignment_statement();
	Tree parse_if_statement();
	Tree parse_while_statement();
	Tree parse_for_statement();
	Tree parse_read_statement();
	Tree parse_write_statement();
	Tree parse_expression();
	Tree parse_expression_naming_variable();
	Tree parse_boolean_expression();
	Tree parse_integer_expression();



	typedef Tree (Parser::*BinaryHandler)(const_TokenPtr, Tree);
	BinaryHandler get_binary_handler(TokenId id);



#define BINARY_HANDLER_LIST                                                    \
  BINARY_HANDLER (plus, PLUS)                                                  \
  BINARY_HANDLER (minus, MINUS)                                                \
  BINARY_HANDLER (mult, ASTERISK)                                              \
  BINARY_HANDLER (div, SLASH)                                                  \
  BINARY_HANDLER (mod, PERCENT)                                                \
  BINARY_HANDLER (equal, EQUAL)                                                \
  BINARY_HANDLER (different, DIFFERENT)                                        \
  BINARY_HANDLER (lower_than, LOWER)                                           \
  BINARY_HANDLER (lower_equal, LOWER_OR_EQUAL)                                 \
  BINARY_HANDLER (greater_than, GREATER)                                       \
  BINARY_HANDLER (greater_equal, GREATER_OR_EQUAL)                             \
  BINARY_HANDLER (logical_and, AND)                                            \
  BINARY_HANDLER (logical_or, OR)                                              \
  BINARY_HANDLER (array_ref, LEFT_SQUARE)                                      \
  BINARY_HANDLER (field_ref, DOT)



#define BINARY_HANDLER(name, _)                                                \
  Tree binary_##name (const_TokenPtr tok, Tree left);
  BINARY_HANDLER_LIST
#undef BINARY_HANDLER



	Lexer &m_lexer;
	ScopeManager m_scope_manager;
	ErrorManager m_error_manager;


  	tree main_fndecl;
  	Tree puts_fn;
  	Tree printf_fn;
  	Tree scanf_fn;


	Tree get_printf_addr();
	Tree get_puts_addr();
	Tree get_scanf_addr();



	public:
  	Parser (Lexer &lexer) : m_lexer(lexer) {}
	void parse_program();


};




void Parser::SkipAfterToken(AAELang::TokenId token_id)
{

	const_TokenPtr token = m_lexer.peek_token();
	while (token->get_id() != AAELang::END_OF_FILE && token->get_id() != token_id)
	{
		m_lexer.skip_token();
		token = m_lexer.peek_token();
	}

	if (token->get_id() == token_id)
		m_lexer.skip_token();

}





const_TokenPtr Parser::ExpectToken(AAELang::TokenId token_id)
{

	const_TokenPtr token = m_lexer.peek_token();
	if (token->get_id() == token_id)
	{
		m_lexer.skip_token();
		return token;
	}
	else
	{
		error_at(token->get_locus(), "expecting %s but %s found\n",
		get_token_description(token_id), token->get_token_description());
		return const_TokenPtr();
	}

}



bool Parser::SkipToken(AAELang::TokenId token_id)
{
	return ExpectToken(token_id) != const_TokenPtr();
}




void Parser::UnexpectedToken(const_TokenPtr token)
{
	error_at(token->get_locus(), "unexpected %s\n", token->get_token_description());
}



void Parser::parse_program()
{


	tree main_fndecl_type = build_function_type_array(integer_type_node, 0, NULL);
	main_fndecl = build_fn_decl("main", main_fndecl_type);
	tree result_declaration = build_decl(UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
	DECL_CONTEXT(result_declaration) = main_fndecl;
	DECL_RESULT(main_fndecl) = result_declaration;
	tree set_return_value = build2(INIT_EXPR, void_type_node, result_declaration, build_int_cst_type(integer_type_node, 0));
	tree return_stmt = build1(RETURN_EXPR, void_type_node, set_return_value);


	m_scope_manager.NewScope();
	parse_statement_sequence(&Parser::done_end_of_file);
	m_scope_manager.GetScopeStatementList().append(return_stmt);
	ScopeManager::ScopeTree main_scope_tree = m_scope_manager.GetAndPopCurrentScope();
	Tree main_block = main_scope_tree.block;


	BLOCK_SUPERCONTEXT(main_block.get_tree()) = main_fndecl;
	DECL_INITIAL(main_fndecl) = main_block.get_tree();
	DECL_SAVED_TREE(main_fndecl) = main_scope_tree.bind_expression.get_tree();


 	DECL_EXTERNAL(main_fndecl) = 0;
  	DECL_PRESERVE_P(main_fndecl) = 1;


	gimplify_function_tree(main_fndecl);
	cgraph_node::finalize_function(main_fndecl, true);

}



bool Parser::done_end_of_file()
{
	const_TokenPtr token = m_lexer.peek_token();
	return (token->get_id() == AAELang::END_OF_FILE);
}



bool Parser::done_brace()
{
	const_TokenPtr token = m_lexer.peek_token();
	return (token->get_id() == AAELang::END_OF_FILE || token->get_id() == AAELang::RIGHT_BRACE);
}



void Parser::parse_statement_sequence(bool (Parser::*done)())
{

	while (!(this->*done) ())
	{
		Tree statement = parse_statement();
		m_scope_manager.GetScopeStatementList().append(statement);
	}

}



Tree Parser::parse_statement()
{
  
	const_TokenPtr token = m_lexer.peek_token();
	switch (token->get_id())
	{
		case AAELang::VAR:
			return parse_variable_declaration();
		case AAELang::TYPE:
      			return parse_type_declaration();
		case AAELang::IF:
			return parse_if_statement();
		case AAELang::WHILE:
			return parse_while_statement();
		case AAELang::FOR:
			return parse_for_statement();
		case AAELang::READ:
			return parse_read_statement();
		case AAELang::WRITE:
			return parse_write_statement();
		case AAELang::IDENTIFIER:
			return parse_assignment_statement();
		default:
			UnexpectedToken(token);
			SkipAfterToken(AAELang::SEMICOLON);
			return Tree::error();
	}
  	gcc_unreachable();

}



void Parser::parse_statement_or_block()
{

	if (m_lexer.peek_token()->get_id() == AAELang::LEFT_BRACE)
	{
		SkipToken(LEFT_BRACE);
		parse_statement_sequence(&Parser::done_brace);
		SkipToken(RIGHT_BRACE);
	}
	else
	{
		Tree statement = parse_statement();
		m_scope_manager.GetScopeStatementList().append(statement);
	}
}



Tree Parser::parse_variable_declaration()
{


	if (!SkipToken(AAELang::VAR))
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	const_TokenPtr identifier = ExpectToken(AAELang::IDENTIFIER);
	if (!identifier)
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	if (!SkipToken(AAELang::COLON))
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	Tree type_tree = parse_type();
	if (type_tree.is_error())
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	if (m_scope_manager.IsSymbolInCurrentScope(identifier->get_str()))
	{
		SetCompileError(AAELang::symbol_already_in_this_scope);
		m_error_manager.IssueError(identifier->get_locus(), identifier->get_str().c_str());
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	SymbolPtr symbol(new Symbol(AAELang::VARIABLE, identifier->get_str()));
	m_scope_manager.InsertSymbolIntoCurrentScope(symbol);


	Tree variable_declaration = build_decl(identifier->get_locus(), VAR_DECL,
			get_identifier(symbol->get_name().c_str()),
			type_tree.get_tree());


	gcc_assert(!m_scope_manager.IsVariableDeclarationChainStackEmpty());
	m_scope_manager.GetScopeVariableDeclarationChain().append(variable_declaration);
	symbol->set_tree_decl(variable_declaration);


	TreeStmtList statement_list;	
	Tree variable_declaration_statement = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, variable_declaration);
	statement_list.append(variable_declaration_statement);



	if (m_lexer.peek_token()->get_id() == AAELang::ASSIG)
	{

		const_TokenPtr assig_token = ExpectToken(AAELang::ASSIG);
		const_TokenPtr first_of_expression = m_lexer.peek_token();
		Tree expression = parse_expression();


		if (expression.is_error())
		{
			SkipAfterToken(AAELang::SEMICOLON);
			return Tree::error();
		}

		/**
		*Convert expression to the type of the variable
		*/
		if (variable_declaration.get_type() != expression.get_type())
			expression = Tree(convert(variable_declaration.get_type().get_tree(), expression.get_tree()), first_of_expression->get_locus());


		Tree assignment_statement = build_tree(MODIFY_EXPR, assig_token->get_locus(), void_type_node, variable_declaration, expression);
		statement_list.append(assignment_statement);

	}
	SkipToken(AAELang::SEMICOLON);
	return statement_list.get_tree();

}



Tree Parser::parse_assignment_statement()
{


	Tree variable = parse_expression_naming_variable();
	if (variable.is_error())
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	const_TokenPtr assig_token = ExpectToken(AAELang::ASSIG);
	if (!assig_token)
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	const_TokenPtr first_of_expression = m_lexer.peek_token();
	Tree expression = parse_expression();
	if (expression.is_error())
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	SkipToken(AAELang::SEMICOLON);
	/**
	*Convert expression to the type of the variable
	*/
	if (variable.get_type() != expression.get_type())
		expression = Tree(convert(variable.get_type().get_tree(), expression.get_tree()), first_of_expression->get_locus());


	Tree assig_expression = build_tree(MODIFY_EXPR, assig_token->get_locus(), void_type_node, variable, expression);
	return assig_expression;

}



Tree Parser::parse_type_declaration()
{


	if (!SkipToken(AAELang::TYPE))
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	const_TokenPtr identifier = ExpectToken(AAELang::IDENTIFIER);
	if (!identifier)
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	if (!SkipToken(AAELang::COLON))
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	Tree type_tree = parse_type();
	if (type_tree.is_error())
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	SkipToken(AAELang::SEMICOLON);
	if (m_scope_manager.IsSymbolInCurrentScope(identifier->get_str()))
	{
		SetCompileError(AAELang::symbol_already_in_this_scope);
		m_error_manager.IssueError(identifier->get_locus(), identifier->get_str().c_str());
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	SymbolPtr symbol(new Symbol(AAELang::TYPENAME, identifier->get_str()));
	m_scope_manager.InsertSymbolIntoCurrentScope(symbol);


	Tree type_declaration = build_decl(identifier->get_locus(), TYPE_DECL,
			  get_identifier(symbol->get_name().c_str()),
			  type_tree.get_tree());


	gcc_assert(!m_scope_manager.IsVariableDeclarationChainStackEmpty());
	m_scope_manager.GetScopeVariableDeclarationChain().append(type_declaration);
	symbol->set_tree_decl(type_declaration);


	Tree statement = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, type_declaration);
	return statement;
  

}



namespace
{


	bool is_string_type(Tree type)
	{
		gcc_assert(TYPE_P(type.get_tree()));
		return type.get_tree_code() == POINTER_TYPE
			&& TYPE_MAIN_VARIANT(TREE_TYPE(type.get_tree())) == char_type_node;
	}


	bool is_array_type(Tree type)
	{
		gcc_assert(TYPE_P(type.get_tree()));
		return type.get_tree_code() == ARRAY_TYPE;
	}


	bool is_record_type(Tree type)
	{
		gcc_assert(TYPE_P(type.get_tree()));
		return type.get_tree_code() == RECORD_TYPE;
	}


}



const char * Parser::print_type(Tree type)
{

	gcc_assert(TYPE_P(type.get_tree()));
	if (type == void_type_node)
		return "void";
	else if (type == char_type_node)
		return "char";
	else if (type == integer_type_node)
		return "int";
	else if (type == float_type_node)
		return "float";
	else if (is_string_type(type))
		return "string";
	else if (is_array_type(type))
		return "array";
	else if (type == boolean_type_node)
		return "boolean";
	else
		return "<<unknown-type>>";
}



Tree Parser::parse_field_declaration(std::vector<std::string> &field_names)
{
  
	const_TokenPtr identifier = ExpectToken(AAELang::IDENTIFIER);
	if (identifier == NULL)
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}


	SkipToken(AAELang::COLON);
	Tree type = parse_type();
	SkipToken(AAELang::SEMICOLON);


  	if (type.is_error())
    		return Tree::error();

	if (std::find(field_names.begin(), field_names.end(), identifier->get_str()) != field_names.end())
	{
		error_at(identifier->get_locus(), "repeated field name!");
		return Tree::error();
	}
	field_names.push_back(identifier->get_str());


  	Tree field_decl = build_decl(identifier->get_locus(), FIELD_DECL,
		  get_identifier(identifier->get_str().c_str()),
		  type.get_tree());


	TREE_ADDRESSABLE(field_decl.get_tree()) = 1;
	return field_decl;
  

}



Tree Parser::parse_record()
{
  

	const_TokenPtr record_token = ExpectToken(AAELang::RECORD);
	if (record_token == NULL)
	{
		SkipAfterToken(AAELang::SEMICOLON);
		return Tree::error();
	}
	SkipToken(AAELang::LEFT_BRACE);


	Tree record_type = make_node(RECORD_TYPE);
	Tree field_list, field_last;
	std::vector<std::string> field_names;


	const_TokenPtr next = m_lexer.peek_token();
	while (next->get_id() != AAELang::RIGHT_BRACE)
	{

		Tree field_decl = parse_field_declaration(field_names);
		if (!field_decl.is_error())
		{
			DECL_CONTEXT(field_decl.get_tree()) = record_type.get_tree();
			if (field_list.is_null())
				field_list = field_decl;
			if (!field_last.is_null())
				TREE_CHAIN(field_last.get_tree()) = field_decl.get_tree();
			field_last = field_decl;
		}
		next = m_lexer.peek_token();
	}


	SkipToken(AAELang::RIGHT_BRACE);
	TYPE_FIELDS(record_type.get_tree()) = field_list.get_tree();
	layout_type(record_type.get_tree());
	return record_type;

}



Tree Parser::parse_type()
{

	
	const_TokenPtr token = m_lexer.peek_token();
	Tree type;

	
	switch (token->get_id())
	{
		case AAELang::CHAR:
			type = signed_char_type_node;
			m_lexer.skip_token();
			break;
		case AAELang::INT:
			type = integer_type_node;
			m_lexer.skip_token();
			break;
		case AAELang::FLOAT:
			type = float_type_node;
			m_lexer.skip_token();
			break;
		case AAELang::BOOL:
			type = boolean_type_node;
			m_lexer.skip_token();
			break;
		case AAELang::IDENTIFIER:
			{
				SymbolPtr symbol = m_scope_manager.GetFirstTypeInScope(token->get_str());
				m_error_manager.IssueError(token->get_locus(), token->get_str().c_str());
				type = (!symbol) ? Tree::error() : TREE_TYPE(symbol->get_tree_decl().get_tree());
				m_lexer.skip_token();
			} break;
		case AAELang::RECORD:
				type = parse_record();
				break;
		default:
				UnexpectedToken(token);
				return Tree::error();
				break;
	}


	typedef std::vector<std::pair<Tree, Tree> > Dimensions;
	Dimensions dimensions;


	token = m_lexer.peek_token();
	while (token->get_id() == AAELang::LEFT_PAREN || token->get_id() == AAELang::LEFT_SQUARE)
	{

		m_lexer.skip_token();
		Tree lower_bound, upper_bound;
		if (token->get_id() == AAELang::LEFT_SQUARE)
		{
		
			Tree size = parse_integer_expression();
			SkipToken(AAELang::RIGHT_SQUARE);


			lower_bound = Tree(build_int_cst_type(integer_type_node, 0), size.get_locus());
	  		upper_bound = build_tree(MINUS_EXPR, size.get_locus(), integer_type_node,
					size, build_int_cst(integer_type_node, 1));

		}
		else if (token->get_id() == AAELang::LEFT_PAREN)
		{
			
			lower_bound = parse_integer_expression();
			SkipToken(AAELang::COLON);
			upper_bound = parse_integer_expression();
			SkipToken(AAELang::RIGHT_PAREN);
		}

		
		dimensions.push_back(std::make_pair(lower_bound, upper_bound));
		token = m_lexer.peek_token();

    }


	for (Dimensions::reverse_iterator it = dimensions.rbegin(); it != dimensions.rend(); it++)
	{
		
		it->first = Tree(fold(it->first.get_tree()), it->first.get_locus());
		it->second = Tree(fold(it->second.get_tree()), it->second.get_locus());

		if (!type.is_error())
		{
			Tree range_type = build_range_type(integer_type_node, it->first.get_tree(), it->second.get_tree());
			type = build_array_type(type.get_tree(), range_type.get_tree());
		}
	}
	return type;

}




Tree Parser::build_label_decl(const char *name, location_t loc)
{

	tree t = build_decl(loc, LABEL_DECL, get_identifier(name), void_type_node);
	gcc_assert(main_fndecl != NULL_TREE);
	DECL_CONTEXT(t) = main_fndecl;

	return t;
}



Tree Parser::build_if_statement(Tree bool_expr, Tree then_part, Tree else_part)
{


	if (bool_expr.is_error())
		return Tree::error();


	Tree then_label_decl = build_label_decl("then", then_part.get_locus());
	Tree else_label_decl = else_part.is_null() ? Tree() : build_label_decl("else", else_part.get_locus());
	Tree endif_label_decl = build_label_decl("end_if", then_part.get_locus());


	Tree goto_then = build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, then_label_decl);
	Tree goto_endif = build_tree(GOTO_EXPR, bool_expr.get_locus (), void_type_node, endif_label_decl);
	Tree goto_else_or_endif = else_part.is_null() ? goto_endif : build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, else_label_decl); 


	Tree cond_expr  = build_tree(COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr, goto_then, goto_else_or_endif);
	Tree then_label_expr = build_tree(LABEL_EXPR, then_part.get_locus(), void_type_node, then_label_decl);


	TreeStmtList stmt_list;
	stmt_list.append(cond_expr);
	stmt_list.append(then_label_expr);
	stmt_list.append(then_part);


	if (!else_part.is_null())
	{
		stmt_list.append(goto_endif);
		Tree else_label_expr = build_tree(LABEL_EXPR, else_part.get_locus(), void_type_node, else_label_decl);
		stmt_list.append(else_label_expr);
		stmt_list.append(else_part);
	}


	Tree endif_label_expr = build_tree(LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, endif_label_decl);
	stmt_list.append(endif_label_expr);
	return stmt_list.get_tree();


}



Tree Parser::parse_if_statement()
{


	if (!SkipToken(AAELang::IF))
		return Tree::error();


	
	Tree expression = parse_boolean_expression();
	m_scope_manager.NewScope();
	parse_statement_or_block();
	Tree then_statement = m_scope_manager.GetAndPopCurrentScope().bind_expression;
	Tree else_statement = Tree();



	if (m_lexer.peek_token()->get_id() == AAELang::ELSE)
	{


		SkipToken(AAELang::ELSE);
		m_scope_manager.NewScope();
		parse_statement_or_block();
		else_statement = m_scope_manager.GetAndPopCurrentScope().bind_expression;


	}
	return build_if_statement(expression, then_statement, else_statement);
}




Tree Parser::build_while_statement(Tree bool_expr, Tree while_body)
{


	if (bool_expr.is_error())
		return Tree::error();


	TreeStmtList stmt_list;
	Tree while_check_label_decl = build_label_decl("while_check", bool_expr.get_locus());
	Tree goto_check = build_tree(GOTO_EXPR, UNKNOWN_LOCATION, void_type_node, while_check_label_decl);
	Tree while_check_label_expr = build_tree(LABEL_EXPR, bool_expr.get_locus(), void_type_node, while_check_label_decl);
	stmt_list.append(while_check_label_expr);


	Tree while_body_label_decl = build_label_decl("while_body", while_body.get_locus());
	Tree end_of_while_label_decl = build_label_decl("end_of_while", UNKNOWN_LOCATION);


	Tree cond_expr = build_tree(COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr,
			build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, while_body_label_decl),
			build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, end_of_while_label_decl));
	stmt_list.append(cond_expr);


	Tree while_body_label_expr = build_tree(LABEL_EXPR, while_body.get_locus(), void_type_node, while_body_label_decl);
	stmt_list.append(while_body_label_expr);
	stmt_list.append(while_body);
	stmt_list.append(goto_check);


	Tree end_of_while_label_expr = build_tree(LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, end_of_while_label_decl);
	stmt_list.append(end_of_while_label_expr);


	return stmt_list.get_tree();

}



Tree Parser::parse_while_statement()
{


	if (!SkipToken(AAELang::WHILE))
    		return Tree::error();


	Tree expression = parse_boolean_expression();
	m_scope_manager.NewScope();
	parse_statement_or_block();
	Tree while_body_statement = m_scope_manager.GetAndPopCurrentScope().bind_expression;


	return build_while_statement(expression, while_body_statement);

}




Tree Parser::build_for_statement(SymbolPtr ind_var, Tree lower_bound, Tree upper_bound, Tree for_body_stmt_list)
{


  if (ind_var == NULL)
    return Tree::error();
  Tree ind_var_decl = ind_var->get_tree_decl();

  // Lower
  if (lower_bound.is_error ())
    return Tree::error ();

  // Upper
  if (upper_bound.is_error ())
    return Tree::error ();

  // ind_var := lower;
  TreeStmtList stmt_list;

  Tree init_ind_var = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION,
				  void_type_node, ind_var_decl, lower_bound);
  stmt_list.append (init_ind_var);

  // ind_var <= upper
  Tree while_condition
    = build_tree (LE_EXPR, upper_bound.get_locus (), boolean_type_node,
		  ind_var_decl, upper_bound);

  // for-body
  // ind_var := ind_var + 1
  Tree incr_ind_var
    = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION, void_type_node,
		  ind_var_decl,
		  build_tree (PLUS_EXPR, UNKNOWN_LOCATION, integer_type_node,
			      ind_var_decl,
			      build_int_cst_type (::integer_type_node, 1)));

  // Wrap as a stmt list
  TreeStmtList for_stmt_list = for_body_stmt_list;
  for_stmt_list.append (incr_ind_var);

  // construct the associated while statement
  Tree while_stmt
    = build_while_statement (while_condition, for_stmt_list.get_tree ());
  stmt_list.append (while_stmt);

  return stmt_list.get_tree ();
}




Tree Parser::parse_for_statement()
{


	if (!SkipToken(AAELang::FOR))
	{
		SkipAfterToken(AAELang::RIGHT_BRACE);
		return Tree::error();
    	}


	const_TokenPtr identifier = ExpectToken(AAELang::IDENTIFIER);
	if(identifier == NULL)
	{
		SkipAfterToken(AAELang::RIGHT_BRACE);
		return Tree::error();
    	}


	if(!SkipToken(AAELang::ASSIG))
	{
		SkipAfterToken(AAELang::RIGHT_BRACE);
		return Tree::error();
   	 }


	Tree lower_bound = parse_integer_expression();
	if (!SkipToken(AAELang::TO))
   	 {
    		SkipAfterToken(AAELang::RIGHT_BRACE);
    		return Tree::error();
    	}


	Tree upper_bound = parse_integer_expression();
   	m_scope_manager.NewScope();
	parse_statement_or_block();
    	Tree for_body_statement = m_scope_manager.GetAndPopCurrentScope().bind_expression;


	SymbolPtr ind_var = m_scope_manager.GetFirstVariableInScope(identifier->get_str());
	m_error_manager.IssueError(identifier->get_locus(), identifier->get_str().c_str());


	return build_for_statement(ind_var, lower_bound, upper_bound, for_body_statement);

}


Tree
Parser::get_scanf_addr ()
{
  if (scanf_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      tree scanf_fn_decl = build_fn_decl ("scanf", fndecl_type);
      DECL_EXTERNAL (scanf_fn_decl) = 1;

      scanf_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), scanf_fn_decl);
    }

  return scanf_fn;
}

Tree
Parser::parse_read_statement ()
{
  if (!SkipToken(AAELang::READ))
    {
      SkipAfterToken(AAELang::SEMICOLON);
      return Tree::error ();
    }

  const_TokenPtr first_of_expr = m_lexer.peek_token ();
  Tree expr = parse_expression_naming_variable ();

  SkipToken(AAELang::SEMICOLON);

  if (expr.is_error ())
    return Tree::error ();

  // Now this variable must be addressable
  TREE_ADDRESSABLE (expr.get_tree ()) = 1;

  const char *format = NULL;
  if (expr.get_type () == integer_type_node)
    {
      format = "%d";
    }
  else if (expr.get_type () == float_type_node)
    {
      format = "%f";
    }
  else
    {
      error_at (first_of_expr->get_locus (),
		"variable of type %s is not a valid read operand",
		print_type (expr.get_type ()));
      return Tree::error ();
    }

  tree args[]
    = {build_string_literal (strlen (format) + 1, format),
       // FIXME
       build_tree (ADDR_EXPR, first_of_expr->get_locus (),
		   build_pointer_type (expr.get_type ().get_tree ()), expr)
	 .get_tree ()};

  Tree scanf_fn = get_scanf_addr ();

  tree stmt
    = build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
			    scanf_fn.get_tree (), 2, args);

  return stmt;
}

Tree
Parser::get_puts_addr ()
{
  if (puts_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (integer_type_node, 1, fndecl_type_param);

      tree puts_fn_decl = build_fn_decl ("puts", fndecl_type);
      DECL_EXTERNAL (puts_fn_decl) = 1;

      puts_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), puts_fn_decl);
    }

  return puts_fn;
}

Tree
Parser::get_printf_addr ()
{
  if (printf_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      tree printf_fn_decl = build_fn_decl ("printf", fndecl_type);
      DECL_EXTERNAL (printf_fn_decl) = 1;

      printf_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), printf_fn_decl);
    }

  return printf_fn;
}

Tree
Parser::parse_write_statement ()
{
  // write_statement -> "write" expression ";"

  if (!SkipToken(AAELang::WRITE))
    {
      SkipAfterToken(AAELang::SEMICOLON);
      return Tree::error ();
    }

  const_TokenPtr first_of_expr = m_lexer.peek_token ();
  Tree expr = parse_expression ();

  SkipToken(AAELang::SEMICOLON);

  if (expr.is_error ())
    return Tree::error ();

  if (expr.get_type () == integer_type_node)
    {
      // printf("%d\n", expr)
      const char *format_integer = "%d\n";
      tree args[]
	= {build_string_literal (strlen (format_integer) + 1, format_integer),
	   expr.get_tree ()};

      Tree printf_fn = get_printf_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				printf_fn.get_tree (), 2, args);

      return stmt;
    }
  else if (expr.get_type () == float_type_node)
    {
      // printf("%f\n", (double)expr)
      const char *format_float = "%f\n";
      tree args[]
	= {build_string_literal (strlen (format_float) + 1, format_float),
	   convert (double_type_node, expr.get_tree ())};

      Tree printf_fn = get_printf_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				printf_fn.get_tree (), 2, args);

      return stmt;
    }
  else if (is_string_type (expr.get_type ()))
    {
      // Alternatively we could use printf('%s\n', expr) instead of puts(expr)
      tree args[] = {expr.get_tree ()};

      Tree puts_fn = get_puts_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				puts_fn.get_tree (), 1, args);
      return stmt;
    }
  else
    {
      error_at (first_of_expr->get_locus (),
		"value of type %s is not a valid write operand",
		print_type (expr.get_type ()));
      return Tree::error ();
    }

  gcc_unreachable();
}




Tree Parser::parse_expression(int right_binding_power)
{

	const_TokenPtr current_token = m_lexer.peek_token();
	m_lexer.skip_token();
	Tree expr = null_denotation(current_token);


	if (expr.is_error())
		return Tree::error();


	while (right_binding_power < left_binding_power(m_lexer.peek_token()))
	{
		current_token = m_lexer.peek_token();
		m_lexer.skip_token();

		expr = left_denotation(current_token, expr);
		if (expr.is_error())
			return Tree::error ();
	}
	return expr;

}



namespace
{


	enum binding_powers
	{
		  LBP_HIGHEST = 100,
		  LBP_DOT = 90,
		  LBP_ARRAY_REF = 80,
		  LBP_UNARY_PLUS = 50, 
		  LBP_UNARY_MINUS = LBP_UNARY_PLUS,
		  LBP_MUL = 40,
		  LBP_DIV = LBP_MUL,
		  LBP_MOD = LBP_MUL,
		  LBP_PLUS = 30,
		  LBP_MINUS = LBP_PLUS,
		  LBP_EQUAL = 20,
		  LBP_DIFFERENT = LBP_EQUAL,
		  LBP_LOWER_THAN = LBP_EQUAL,
		  LBP_LOWER_EQUAL = LBP_EQUAL,
		  LBP_GREATER_THAN = LBP_EQUAL,
		  LBP_GREATER_EQUAL = LBP_EQUAL,
		  LBP_LOGICAL_AND = 10,
		  LBP_LOGICAL_OR = LBP_LOGICAL_AND,
		  LBP_LOGICAL_NOT = LBP_LOGICAL_AND,
		  LBP_LOWEST = 0,
	};


}



int Parser::left_binding_power(const_TokenPtr token)
{

	switch (token->get_id())
	{
	case AAELang::DOT:
		return LBP_DOT;
	case AAELang::LEFT_SQUARE:
		return LBP_ARRAY_REF;
	case AAELang::ASTERISK:
		return LBP_MUL;
	case AAELang::SLASH:
		return LBP_DIV;
	case AAELang::PERCENT:
		return LBP_MOD;
	case AAELang::PLUS:
		return LBP_PLUS;
	case AAELang::MINUS:
		return LBP_MINUS;
	case AAELang::EQUAL:
		return LBP_EQUAL;
	case AAELang::DIFFERENT:
		return LBP_DIFFERENT;
	case AAELang::GREATER:
		return LBP_GREATER_THAN;
	case AAELang::GREATER_OR_EQUAL:
		return LBP_GREATER_EQUAL;
	case AAELang::LOWER:
		return LBP_LOWER_THAN;
	case AAELang::LOWER_OR_EQUAL:
		return LBP_LOWER_EQUAL;
	case AAELang::OR:
		return LBP_LOGICAL_OR;
	case AAELang::AND:
		return LBP_LOGICAL_AND;
	case AAELang::NOT:
		return LBP_LOGICAL_NOT;
	default:
		return LBP_LOWEST;
	}

}


Tree Parser::null_denotation(const_TokenPtr tok)
{

	switch (tok->get_id())
	{
		case AAELang::IDENTIFIER:
			{
				SymbolPtr symbol = m_scope_manager.GetFirstVariableInScope(tok->get_str());
				m_error_manager.IssueError(tok->get_locus(), tok->get_str().c_str());
				return (!symbol) ? Tree::error() : Tree(symbol->get_tree_decl(), tok->get_locus());
			}
		case AAELang::INTEGER_LITERAL: /** FIXME : check ranges */
			{
				return Tree(build_int_cst_type(integer_type_node, atoi(tok->get_str().c_str())), tok->get_locus());
			}
		case AAELang::REAL_LITERAL:
			{
				REAL_VALUE_TYPE real_value;
				real_from_string3(&real_value, tok->get_str().c_str(), TYPE_MODE(float_type_node));
				return Tree(build_real(float_type_node, real_value), tok->get_locus());
			}
		case AAELang::STRING_LITERAL:
			{
				std::string str = tok->get_str();
				const char *c_str = str.c_str();
				return Tree(build_string_literal(strlen(c_str) + 1, c_str), tok->get_locus());
			}
		case AAELang::TRUE_LITERAL:
			{
				return Tree(build_int_cst_type(boolean_type_node, 1), tok->get_locus());
			}
		case AAELang::FALSE_LITERAL:
			{
				return Tree(build_int_cst_type(boolean_type_node, 0), tok->get_locus());
			}
		case AAELang::LEFT_PAREN:
			{
				Tree expr = parse_expression();
				tok = m_lexer.peek_token();
				if (tok->get_id() != AAELang::RIGHT_PAREN)
					error_at(tok->get_locus(), "expecting ) but %s found\n",
							tok->get_token_description());
				else
					m_lexer.skip_token();
				return Tree(expr, tok->get_locus());
			}
		case AAELang::PLUS:
			{
				Tree expr = parse_expression(LBP_UNARY_PLUS);
				if (expr.is_error())
					return Tree::error();
				if (expr.get_type() != integer_type_node && expr.get_type() != float_type_node)
				{
					error_at(tok->get_locus(), "operand of unary plus must be int or float but it is %s",
							print_type(expr.get_type()));
					return Tree::error();
				}
				return Tree(expr, tok->get_locus());
			}
		case AAELang::MINUS:
			{
				Tree expr = parse_expression(LBP_UNARY_MINUS);
				if (expr.is_error())
					return Tree::error();
				if (expr.get_type() != integer_type_node && expr.get_type() != float_type_node)
				{
					error_at(tok->get_locus(), "operand of unary minus must be int or float but it is %s",
							print_type(expr.get_type()));
					return Tree::error();
				}
				expr = build_tree(NEGATE_EXPR, tok->get_locus(), expr.get_type(), expr);
				return expr;
			}
		case AAELang::NOT:
			{
				Tree expr = parse_expression(LBP_LOGICAL_NOT);
				if (expr.is_error())
					return Tree::error();
				if (expr.get_type() != boolean_type_node)
				{
					error_at(tok->get_locus(), "operand of logical not must be a boolean but it is %s",
							print_type(expr.get_type()));
					return Tree::error ();
				}
				expr = build_tree(TRUTH_NOT_EXPR, tok->get_locus(), boolean_type_node, expr);
				return expr;
			}
		default:
			UnexpectedToken(tok);
			return Tree::error();
	}

}



Tree Parser::coerce_binary_arithmetic(const_TokenPtr tok, Tree *left, Tree *right)
{


	Tree left_type = left->get_type();
	Tree right_type = right->get_type();


	if (left_type.is_error() || right_type.is_error())
		return Tree::error();


	if (left_type == right_type)
	{
		if (left_type == integer_type_node || left_type == float_type_node)
		{
			return left_type;
		}
	}
	else if ((left_type == integer_type_node && right_type == float_type_node) || 
			(left_type == float_type_node && right_type == integer_type_node))
	{
		if (left_type == integer_type_node)
		{
			*left = build_tree(FLOAT_EXPR, left->get_locus(), float_type_node, left->get_tree());
		}
		else
		{
			*right = build_tree(FLOAT_EXPR, right->get_locus(), float_type_node, right->get_tree());
		}
		return float_type_node;
	}

	
	error_at(tok->get_locus(),
			"invalid operands of type %s and %s for operator %s",
			print_type(left_type), print_type(right_type),
			tok->get_token_description());
	return Tree::error();

}



Parser::BinaryHandler Parser::get_binary_handler(TokenId id)
{

	switch (id)
	{
		#define BINARY_HANDLER(name, token_id)				\
		case AAELang::token_id:						\
		return &Parser::binary_##name;
		BINARY_HANDLER_LIST
		#undef BINARY_HANDLER
		default:
		return NULL;
	}

}



Tree Parser::binary_plus(const_TokenPtr tok, Tree left)
{

	Tree right = parse_expression(LBP_PLUS);
	if (right.is_error())
		return Tree::error();
	
	Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
	if (tree_type.is_error())
		return Tree::error();

	return build_tree(PLUS_EXPR, tok->get_locus(), tree_type, left, right);
}



Tree
Parser::binary_minus (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MINUS);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MINUS_EXPR, tok->get_locus (), tree_type, left, right);
}

Tree
Parser::binary_mult (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MUL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MULT_EXPR, tok->get_locus (), tree_type, left, right);
}

Tree
Parser::binary_div (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_DIV);
  if (right.is_error ())
    return Tree::error ();

  if (left.get_type () == integer_type_node
      && right.get_type () == integer_type_node)
    {
      // Integer division (truncating, like in C)
      return build_tree (TRUNC_DIV_EXPR, tok->get_locus (), integer_type_node,
			 left, right);
    }
  else
    {
      // Real division
      Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
      if (tree_type.is_error ())
	return Tree::error ();

      gcc_assert (tree_type == float_type_node);

      return build_tree (RDIV_EXPR, tok->get_locus (), tree_type, left, right);
    }
}

Tree
Parser::binary_mod (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MOD);
  if (right.is_error ())
    return Tree::error ();

  if (left.get_type () == integer_type_node
      && right.get_type () == integer_type_node)
    {
      // Integer division (truncating, like in C)
      return build_tree (TRUNC_MOD_EXPR, tok->get_locus (), integer_type_node,
			 left, right);
    }
  else
    {
      error_at (tok->get_locus (),
		"operands of modulus must be of integer type");
      return Tree::error ();
    }
}

Tree
Parser::binary_equal (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (EQ_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

Tree
Parser::binary_different (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_DIFFERENT);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (NE_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

Tree
Parser::binary_lower_than (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOWER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LT_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

Tree
Parser::binary_lower_equal (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOWER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LE_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

Tree
Parser::binary_greater_than (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_GREATER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GT_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

Tree
Parser::binary_greater_equal (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_GREATER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GE_EXPR, tok->get_locus (), boolean_type_node, left,
		     right);
}

bool
Parser::check_logical_operands (const_TokenPtr tok, Tree left, Tree right)
{
  if (left.get_type () != boolean_type_node
      || right.get_type () != boolean_type_node)
    {
      error_at (
	tok->get_locus (),
	"operands of operator %s must be boolean but they are %s and %s\n",
	tok->get_token_description (), print_type (left.get_type ()),
	print_type (right.get_type ()));
      return false;
    }

  return true;
}

Tree
Parser::binary_logical_and (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOGICAL_AND);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ANDIF_EXPR, tok->get_locus (), boolean_type_node,
		     left, right);
}

Tree
Parser::binary_logical_or (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOGICAL_OR);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ORIF_EXPR, tok->get_locus (), boolean_type_node,
		     left, right);
}

Tree
Parser::binary_array_ref (const const_TokenPtr tok, Tree left)
{
  Tree right = parse_integer_expression ();
  if (right.is_error ())
    return Tree::error ();

  if (!SkipToken(AAELang::RIGHT_SQUARE))
    return Tree::error ();

  if (!is_array_type (left.get_type ()))
    {
      error_at (left.get_locus(), "does not have array type");
      return Tree::error ();
    }

  Tree element_type = TREE_TYPE(left.get_type().get_tree());

  return build_tree (ARRAY_REF, tok->get_locus (), element_type, left, right, Tree(), Tree());
}

Tree
Parser::binary_field_ref (const const_TokenPtr tok, Tree left)
{
  const_TokenPtr identifier = ExpectToken(AAELang::IDENTIFIER);
  if (identifier == NULL)
    {
      return Tree::error ();
    }

  if (!is_record_type (left.get_type ()))
    {
      error_at (left.get_locus (), "does not have record type");
      return Tree::error ();
    }

  Tree field_decl = TYPE_FIELDS (left.get_type ().get_tree ());
  while (!field_decl.is_null ())
    {
      Tree decl_name = DECL_NAME (field_decl.get_tree ());
      const char *field_name = IDENTIFIER_POINTER (decl_name.get_tree ());

      if (field_name == identifier->get_str ())
	break;

      field_decl = TREE_CHAIN (field_decl.get_tree ());
    }

  if (field_decl.is_null ())
    {
      error_at (left.get_locus (),
		"record type does not have a field named '%s'",
		identifier->get_str ().c_str ());
      return Tree::error ();
    }

  return build_tree (COMPONENT_REF, tok->get_locus (),
		     TREE_TYPE (field_decl.get_tree ()), left, field_decl,
		     Tree ());
}


	
Tree Parser::left_denotation(const_TokenPtr tok, Tree left)
{

	BinaryHandler binary_handler = get_binary_handler(tok->get_id());
	if (binary_handler == NULL)
	{
		UnexpectedToken(tok);
		return Tree::error();
	}
	return (this->*binary_handler)(tok, left);
  
}



Tree Parser::parse_expression()
{
	return parse_expression(LBP_LOWEST);
}



Tree Parser::parse_boolean_expression()
{

	Tree expr = parse_expression();
	if (expr.is_error())
		return expr;

	if (expr.get_type() != boolean_type_node)
	{
		error_at (expr.get_locus(), "expected expression of boolean type but its type is %s", print_type(expr.get_type()));
		return Tree::error();
	}
	return expr;

}



Tree Parser::parse_integer_expression()
{

	Tree expr = parse_expression();
	if (expr.is_error())
		return expr;

	if (expr.get_type() != integer_type_node)
	{
		error_at(expr.get_locus(),
			"expected expression of integer type but its type is %s",
			print_type(expr.get_type()));
		return Tree::error ();
	}
	return expr;

}



Tree Parser::parse_expression_naming_variable()
{

	Tree expr = parse_expression();
	if (expr.is_error())
		return expr;


	if (expr.get_tree_code() != VAR_DECL && 
			expr.get_tree_code() != ARRAY_REF && 
			expr.get_tree_code() != COMPONENT_REF)
	{
		error_at(expr.get_locus(), "does not designate a variable, array element or field");
		return Tree::error();
	}
	return expr;

}



}



static void tiny_parse_file(const char *filename)
{

  
	FILE *file = fopen(filename, "r");
	if (file == NULL)
		fatal_error(UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);


	AAELang::Lexer m_lexer(filename, file);
	AAELang::Parser parser(m_lexer);


	parser.parse_program();
	fclose(file);

}


void tiny_parse_files(int num_files, const char **files)
{

	for (int i = 0; i < num_files; i++)
		tiny_parse_file (files[i]);


}
