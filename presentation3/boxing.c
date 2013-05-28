struct Value {
	enum Type { List, Pair, Int, Bool } type;
	void* data;
};
