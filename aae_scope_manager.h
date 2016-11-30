#ifndef AAE_SCOPE_MANAGER_H
#define AAE_SCOPE_MANAGER_H



#include "tiny/tiny-scope.h"
#include "tiny/tiny-tree.h"
#include "tiny/tiny-symbol.h"
#include "aae_error.h"



namespace AAELang
{
	
	class ScopeManager
	{
	

	private:

		
		Scope m_scope;
	  	std::vector<TreeStmtList> m_statement_list_stack;
		std::vector<TreeChain> m_variable_declaration_chain_stack;
		std::vector<BlockChain> m_block_chain_stack;


	public:


		ScopeManager() {}
		struct ScopeTree
		{	
			Tree bind_expression;
			Tree block;
		};

		inline TreeStmtList &GetScopeStatementList()
		{
			return m_statement_list_stack.back();
		}
		inline TreeChain &GetScopeVariableDeclarationChain()
		{
			return m_variable_declaration_chain_stack.back();
		}
		inline BlockChain &GetScopeBlockChain()
		{
			return m_block_chain_stack.back();
		}
		inline bool IsVariableDeclarationChainStackEmpty()
		{
			return m_variable_declaration_chain_stack.empty();
		}
		inline bool IsSymbolInCurrentScope(const std::string& identifier)
		{
			return m_scope.get_current_mapping().get(identifier);
		}
		inline void InsertSymbolIntoCurrentScope(SymbolPtr symbol)
		{
			m_scope.get_current_mapping().insert(symbol);
		}



		void NewScope();
  		ScopeTree GetAndPopCurrentScope();



		SymbolPtr GetFirstTypeInScope(const std::string&);
		SymbolPtr GetFirstVariableInScope(const std::string&);


	};

}


#endif /** AAE_SCOPE_MANAGER_H **/