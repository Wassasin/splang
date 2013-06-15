#include <ostream>

namespace spl{
	// Quick and dirty spl implementation, hopefully having the same memory layout as the splang compiler outputs...
	// Note: we leak :D
	struct Nil {} nil;

	template <typename T>
	struct list_node{
		list_node * next = nullptr;
		T value;

		list_node(list_node * next, T const & value)
		: next(next)
		, value(value)
		{}
	};

	template <typename T>
	struct list{
		list_node<T> * s;

		list & operator=(list_node<T> * r){ s = r; return *this; }
		operator list_node<T>*(){ return s; }
		list_node<T>& operator*(){ return *s; }
		list_node<T>* operator->(){ return s; }
	};

	template <typename T>
	list<T> operator%=(T const & t, list<T> l){
		return {new list_node<T>{l, t}};
	}

	template <typename T>
	list<T> operator%=(T const & t, Nil){
		return {new list_node<T>{nullptr, t}};
	}

	template <typename T, typename S>
	struct pair{
		T fst;
		S snd;
	};

	template<typename T>
	std::ostream& operator<<(std::ostream& os, list<T> it){
		os << "[";
		while(it){
			os << it->value;
			if(it->next) os << ", ";
			it = it->next;
		}
		return os << "]";
	}

	template<typename T, typename S>
	std::ostream& operator<<(std::ostream& os, pair<T, S> p){
		return os << "(" << p.fst << ", " << p.snd << ")";
	}
}
