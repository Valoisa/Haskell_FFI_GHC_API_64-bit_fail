#include <functional>
#include <windows.h>
#include <iostream>
#include <string>

template <class FunctionType>
void load_function(FunctionType& function, HINSTANCE module, const std::string& function_name)
{
	function = reinterpret_cast<FunctionType>(static_cast<void*>(::GetProcAddress(module, function_name.c_str())));
	if (function == nullptr)
		throw std::exception("Failed to load function.");
}

typedef void(__cdecl *VoidTypeNoArgs)();
typedef void*(__cdecl *VoidPtrTypeNoArgs)();
typedef void*(__cdecl *VoidPtrTypeVoidArg)(void*);
typedef void*(__cdecl *VoidPtrTypeWcharTArg)(wchar_t*);
typedef const void*(__cdecl *VoidPtrTypeVoidWCharTArgs)(void*, const wchar_t*);
typedef const wchar_t*(__cdecl *WcharTTypeVoidArg)(void*);
typedef std::function<void(void*)> stable_ptr_deleter_type;
typedef std::function<void(const wchar_t*)> cwstring_deleter_type;

int main(void)
{
	HINSTANCE module = ::LoadLibrary(L"HSCodeGen.dll");
//	GetLastError
	if (module == NULL)
		throw std::exception("Failed to load HSCodeGen library.\n");
	VoidTypeNoArgs initialize;
	load_function(initialize, module, "HsStart");

	VoidTypeNoArgs finalize;
	load_function(finalize, module, "HsEnd");

	VoidPtrTypeNoArgs create_context;
	load_function(create_context, module, "create_context");

	VoidPtrTypeVoidArg compile;
	load_function(compile, module, "bring_decls_to_context");

	VoidPtrTypeVoidWCharTArgs execute;
	load_function(execute, module, "execute");

	WcharTTypeVoidArg has_exception;
	load_function(has_exception, module, "ghc_has_exception");

	WcharTTypeVoidArg has_pref_error;
	load_function(has_pref_error, module, "has_pref_error");

	WcharTTypeVoidArg show_result;
	load_function(show_result, module, "show_result");

	VoidPtrTypeVoidArg free_stable_ptr;
	load_function(free_stable_ptr, module, "free_stable_ptr");

	VoidPtrTypeWcharTArg free_cwstring;
	load_function(free_cwstring, module, "free_cwstring");

	initialize();
	void* context = create_context();

	void* context_with_decls = compile(context);
	const wchar_t* compile_exc = has_exception(context_with_decls);
	if (std::wstring(compile_exc) != L"")
	{
		std::wcout << std::wstring(compile_exc) << std::endl;
		return 1;
	}
	const void* execution_result = execute(context_with_decls, L"root");
	const wchar_t* execution_exc = has_pref_error(const_cast<void*>(execution_result));
	if (std::wstring(execution_exc) != L"")
	{
		std::wcout << std::wstring(execution_exc) << std::endl;
		return 2;
	}

	const wchar_t* execution_res_str = show_result(const_cast<void*>(execution_result));
	std::wcout << std::wstring(execution_res_str) << std::endl;
	finalize();
	system("pause");

	return 0;
}