#include "aae_error_manager.h"



namespace AAELang
{



	static CompileErrorCode current_error_code;
	static bool should_process_error;




	void SetCompileError(CompileErrorCode m_code)
	{
		current_error_code = m_code;
		should_process_error = true;
	}
	bool AnyCompileError()
	{
		return should_process_error;
	}



	void ErrorManager::IssueError(location_t locus, ...)
	{


		if (!should_process_error)
			return;


		char buffer[256];
		va_list args;
		va_start(args, locus);


		switch (current_error_code)
		{
			case symbol_not_in_this_scope:
				vsprintf(buffer, "Symbol \"%s\" was not declared in this scope!", args);
				break;
			case symbol_already_in_this_scope:
				vsprintf(buffer, "Symbol \"%s\" was already declared in this scope!", args);
				break;
			case symbol_is_not_variable:
				vsprintf(buffer, "Symbol \"%s\" is not a variable!", args);
				break;
			case symbol_is_not_type:
				vsprintf(buffer, "Symbol \"%s\" is not a type!", args);
				break;
			default:
				vsprintf(buffer, "Something happened!", args);
				break;

		}


		error_at(locus, buffer);
		va_end(args);
		should_process_error = false;

	}



}