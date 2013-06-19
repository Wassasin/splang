#include <iostream>
#include <stdexcept>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>

#include "kareltje.hpp"
static game game;

using namespace llvm;
using vvtype = void(*)(void);

Module * load_module(LLVMContext& context, std::string const & filename){
	using namespace llvm;
	SMDiagnostic error;
	auto ret = ParseIRFile(filename, error, context);
	if(!ret) throw std::runtime_error(error.getMessage());
	return ret;
}

vvtype get_main(Module * module){
	auto engine = ExecutionEngine::createJIT(module, nullptr, 0, CodeGenOpt::Aggressive);
	if(!engine) throw std::runtime_error("Failed to construct ExecutionEngine");

	engine->addGlobalMapping(module->getFunction("up"), (void*) (vvtype) []{ game.up(); });
	engine->addGlobalMapping(module->getFunction("left"), (void*) (vvtype) []{ game.left(); });
	engine->addGlobalMapping(module->getFunction("down"), (void*) (vvtype) []{ game.down(); });
	engine->addGlobalMapping(module->getFunction("right"), (void*) (vvtype) []{ game.right(); });

	auto f = (vvtype) engine->getPointerToFunction(module->getFunction("main"));
	if(!f) throw std::runtime_error("Main function could not be found");
	return f;
}

int main(){
	InitializeNativeTarget();
	LLVMContext context;

	vvtype main_function = nullptr;

	while(true) try {
		std::cout << "What should I do? [compile, run, exit]" << std::endl;
		std::string line;
		std::getline(std::cin, line);

		if(line == "compile"){
			auto module = load_module(context, "program.ll");
			main_function = get_main(module);
			std::cout << "Succesfully compiled, ready to run" << std::endl;
		}
		if(line == "run"){
			if(!main_function) throw std::runtime_error("No main function, compile first");
			main_function();
		}
		if(line == "exit") break;
	} catch(std::exception & e) {
		std::cout << "Exception: " << e.what() << std::endl;
	}
}
