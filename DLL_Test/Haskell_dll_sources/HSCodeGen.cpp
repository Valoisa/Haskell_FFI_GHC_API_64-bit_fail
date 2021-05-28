#include "Compilation_stub.h"
#include <Rts.h>

#include <wchar.h>

#define DLLExport extern "C" __declspec(dllexport)

DLLExport void HsStart()
{
    int argc = 1;
    const char* argv[] = {"ghcDLL", NULL};
    
    const char** args = argv;
    hs_init(&argc, const_cast<char***>(&args));
}

DLLExport void HsEnd()
{
    hs_exit();
}

DLLExport void* bring_decls_to_context (void* context)
{
    return c_bring_decls_to_context(context);
}

DLLExport void* create_context ()
{
    return c_create_context();
}

DLLExport const void* execute (void* context, const wchar_t* final_exp)
{
    return reinterpret_cast<const wchar_t*>(c_execute(context, const_cast<void*>(reinterpret_cast<const void*>(final_exp))));
}

DLLExport const wchar_t* ghc_has_exception (void* ctxt)
{
    return reinterpret_cast<const wchar_t*>(c_ghc_has_exception(ctxt));
}

DLLExport const wchar_t* has_pref_error (void* ctxt)
{
    return reinterpret_cast<const wchar_t*>(c_has_pref_error(ctxt));
}

DLLExport const wchar_t* show_result (void* ctxt)
{
    return reinterpret_cast<const wchar_t*>(c_show_result(ctxt));
}

DLLExport void free_stable_ptr (void* ctxt)
{
	hs_free_stable_ptr(ctxt);
	hs_perform_gc();
}

DLLExport void free_cwstring (wchar_t* str)
{
	hs_perform_gc();
	c_free_cwstring(str);
}

