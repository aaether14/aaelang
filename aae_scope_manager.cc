#include "aae_scope_manager.h"


namespace AAELang
{


void ScopeManager::NewScope()
{

	m_scope.push_scope();
	m_statement_list_stack.push_back(TreeStmtList());
	m_variable_declaration_chain_stack.push_back(TreeChain());
	m_block_chain_stack.push_back(BlockChain());

}



ScopeManager::ScopeTree ScopeManager::GetAndPopCurrentScope()
{


	TreeStmtList current_statement_list = GetScopeStatementList();
	m_statement_list_stack.pop_back();


	TreeChain current_variable_declaration_chain = GetScopeVariableDeclarationChain();
	m_variable_declaration_chain_stack.pop_back();


  	BlockChain subblocks = GetScopeBlockChain();
	m_block_chain_stack.pop_back();


	tree new_block = build_block(current_variable_declaration_chain.first.get_tree(), 
			subblocks.first.get_tree(),
			NULL_TREE, NULL_TREE);

  
	if (!m_block_chain_stack.empty())
		GetScopeBlockChain().append(new_block);

  
	for (tree it = subblocks.first.get_tree(); it != NULL_TREE; it = BLOCK_CHAIN(it))
		BLOCK_SUPERCONTEXT(it) = new_block;

	
	tree bind_expression = build3(BIND_EXPR, void_type_node, current_variable_declaration_chain.first.get_tree(),
			current_statement_list.get_tree(), new_block);


	ScopeTree scope_tree;
	scope_tree.bind_expression = bind_expression;
	scope_tree.block = new_block;


	m_scope.pop_scope();
	return scope_tree;

}



SymbolPtr ScopeManager::GetFirstTypeInScope(const std::string &name)
{

	SymbolPtr symbol = m_scope.lookup(name);
	if (!symbol)
		SetCompileError(AAELang::symbol_not_in_this_scope);
	else if (symbol->get_kind() != AAELang::TYPENAME)
	{
		SetCompileError(AAELang::symbol_is_not_type);
		symbol = SymbolPtr();
	}
	return symbol;

}



SymbolPtr ScopeManager::GetFirstVariableInScope(const std::string &name)
{

	SymbolPtr symbol = m_scope.lookup(name);
	if (!symbol)
		SetCompileError(AAELang::symbol_not_in_this_scope);
	else if (symbol->get_kind() != AAELang::VARIABLE)
	{
		SetCompileError(AAELang::symbol_is_not_variable);
		symbol = SymbolPtr();
	}
	return symbol;

}




}