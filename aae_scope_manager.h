#include "tiny/tiny-scope.h"
#include "tiny/tiny-tree.h"


namespace AAELang
{
	
	class ScopeManager
	{
	

	private:
		Scope scope;
	  	std::vector<TreeStmtList> statement_list_stack;
		std::vector<TreeChain> variable_declaration_chain_stack;
		std::vector<BlockChain> block_chain_stack;


	public:

		/**
		*Just a nothing constructor
		*/
		ScopeManager() {}


		inline TreeStmtList &GetScopeStatementList()
		{
			return statement_list_stack.back();
		}
		inline TreeChain &GetScopeVariableDeclarationChain()
		{
			return variable_declaration_chain_stack.back();
		}
		inline BlockChain &GetScopeBlockChain()
		{
			return block_chain_stack.back();
		}
		inline Scope &GetScope()
		{
			return scope;
		}
		inline bool IsVariableDeclarationChainStackEmpty()
		{
			return variable_declaration_chain_stack.empty();
		}


		struct TreeSymbolMapping
		{	
			Tree bind_expr;
			Tree block;
		};
		void enter_scope();
  		TreeSymbolMapping leave_scope();


	};

}