#include <iostream>
#include <stdexcept>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>

#include "compiler.hpp"
#include "kareltje.hpp"
static game game;

using namespace llvm;
using namespace compiler;

int main(){
	InitializeNativeTarget();
	LLVMContext context;

	vvtype main_function = nullptr;

	while(true) try {
		std::cout << "What should I do? [compile, run, exit]" << std::endl;
		std::string line;
		std::getline(std::cin, line);

		if(line == "compile"){
			main_function = compile(context, "program", {
				{"up",		(void*) (vvtype) []{ game.up(); }	},
				{"left",	(void*) (vvtype) []{ game.left(); }	},
				{"down",	(void*) (vvtype) []{ game.down(); }	},
				{"right",	(void*) (vvtype) []{ game.right(); }	}
			});
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
