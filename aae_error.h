#ifndef AAE_ERROR_H
#define AAE_ERROR_H


namespace AAELang
{


	enum CompileErrorCode
	{

		symbol_not_in_this_scope,
		symbol_already_in_this_scope,
		symbol_is_not_variable,
		symbol_is_not_type,

	};



	void SetCompileError(CompileErrorCode);
	bool AnyCompileError();


}


#endif /** AAE_ERROR_H **/