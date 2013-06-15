#include <iostream>

// There is a very basis "spl sdk" ;D!
#include "spl.hpp"

// Note: we have to do the name mangling ourself :(... (and hope for the memory layout to be consistent)
extern "C" int sum_i_li(spl::list<int>);
extern "C" spl::list<int> reverse_li_li(spl::list<int>);
extern "C" spl::list<spl::pair<int,int>> zip_lpii_lili(spl::list<int>, spl::list<int>);

int main(){
	// the spl sdk has some operator overloading :D
	auto my_list = 1 %= 2 %= 3 %= 4 %= 5 %= spl::nil;
	auto re_list = reverse_li_li(my_list);
	std::cout << "original list: " << my_list << std::endl;
	std::cout << "revesed list: " << re_list << std::endl;
	std::cout << "zipped list: " << zip_lpii_lili(my_list, re_list) << std::endl;
	std::cout << "sum of list: " << sum_i_li(my_list) << std::endl;
}
