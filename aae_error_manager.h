#ifndef AAE_ERROR_MANAGER_H
#define AAE_ERROR_MANAGER_H



#include <stdarg.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"
#include "diagnostic.h"
#include "aae_error.h"




namespace AAELang
{


	class ErrorManager
	{


	public:


		ErrorManager() {}
		void IssueError(location_t locus, ...);


	};


}


#endif /** AAE_ERROR_MANAGER_H **/