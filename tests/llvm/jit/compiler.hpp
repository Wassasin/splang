#pragma once

#include <stdexcept>
#include <map>

#include <boost/process.hpp>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>

namespace compiler {

	using namespace llvm;
	using vvtype = void(*)(void);
	using function_map = std::map<std::string, void*>;

	Module * load_module(LLVMContext& context, std::string const & filename){
		using namespace llvm;
		SMDiagnostic error;
		auto ret = ParseIRFile(filename, error, context);
		if(!ret) throw std::runtime_error(error.getMessage());
		return ret;
	}

	vvtype get_main(Module * module, function_map const & functions){
		auto engine = ExecutionEngine::createJIT(module, nullptr, 0, CodeGenOpt::Aggressive);
		if(!engine) throw std::runtime_error("Failed to construct ExecutionEngine");

		for(auto f : functions){
			engine->addGlobalMapping(module->getFunction(f.first), f.second);
		}

		auto f = (vvtype) engine->getPointerToFunction(module->getFunction("main"));
		if(!f) throw std::runtime_error("Main function could not be found");
		return f;
	}

	// filename without .spl extension
	vvtype compile(LLVMContext& context, std::string const & filename, function_map const & functions){
		namespace bp = boost::process;
		namespace bpi = boost::process::initializers;

		std::stringstream args;
		args << "splang --target=llvm -o " << filename << ".ll " << filename << ".spl";

		auto c = bp::execute(
			bpi::run_exe("/Users/joshua/bin/splang"),
			bpi::set_cmd_line(args.str())
		);

		if(bp::wait_for_exit(c)){
			throw std::runtime_error("Splang did not return succesfully");
		} else {
			auto module = load_module(context, filename + ".ll");
			auto main_function = get_main(module, functions);
			return main_function;
		}
	}

}