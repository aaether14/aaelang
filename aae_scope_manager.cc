#include "aae_scope_manager.h"


namespace AAELang
{


void ScopeManager::enter_scope()
{

	scope.push_scope();
	statement_list_stack.push_back(TreeStmtList());
	variable_declaration_chain_stack.push_back(TreeChain());
	block_chain_stack.push_back(BlockChain());

}



ScopeManager::TreeSymbolMapping ScopeManager::leave_scope()
{


	TreeStmtList current_statement_list = GetScopeStatementList();
	statement_list_stack.pop_back();


	TreeChain current_variable_declaration_chain = GetScopeVariableDeclarationChain();
	variable_declaration_chain_stack.pop_back();


  	BlockChain subblocks = GetScopeBlockChain();
	block_chain_stack.pop_back();


	tree new_block = build_block(current_variable_declaration_chain.first.get_tree(), 
			subblocks.first.get_tree(),
			NULL_TREE, NULL_TREE);

  
	if (!block_chain_stack.empty())
		GetScopeBlockChain().append(new_block);

  
	for (tree it = subblocks.first.get_tree(); it != NULL_TREE; it = BLOCK_CHAIN(it))
		BLOCK_SUPERCONTEXT(it) = new_block;

	
	tree bind_expr = build3(BIND_EXPR, void_type_node, current_variable_declaration_chain.first.get_tree(),
			current_statement_list.get_tree(), new_block);


	TreeSymbolMapping tree_scope;
	tree_scope.bind_expr = bind_expr;
	tree_scope.block = new_block;
	scope.pop_scope();


	return tree_scope;

}


}