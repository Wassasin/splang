#pragma once

#include <vector>
#include <thread>
#include "../cpp/spl.hpp"

struct game{
	struct kareltje{
		int x, y;
	} kareltje;

	struct world{
		int width, height;
	} world;

	game()
	: kareltje{0, 0}
	, world{20, 10}
	{}

	void print(){
		std::cout << std::endl << std::string(world.width+2, '#') << std::endl;
		for(int y = 0; y < kareltje.y; ++y){
			std::cout << "#" << std::string(world.width, ' ') << "#" << std::endl;
		}
		std::cout << "#" << std::string(kareltje.x, ' ') << "X" << std::string(world.width - kareltje.x - 1, ' ') << "#" << std::endl;
		for(int y = 0; y < world.height - kareltje.y - 1; ++y){
			std::cout << "#" << std::string(world.width, ' ') << "#" << std::endl;
		}
		std::cout << std::string(world.width+2, '#') << std::endl;

		std::this_thread::sleep_for(std::chrono::milliseconds(100));
	}

	void up(){
		if(kareltje.y > 0) kareltje.y--;
		print();
	}

	void left(){
		if(kareltje.x > 0) kareltje.x--;
		print();
	}

	void down(){
		if(kareltje.y < world.height-1) kareltje.y++;
		print();
	}

	void right(){
		if(kareltje.x < world.width-1) kareltje.x++;
		print();
	}
};
