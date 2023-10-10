Class Main{
	main():Int{
		1
	};
};

Class C inherits B{
	a():SELF_TYPE{
		self.func(y,x)
	};
};

Class B inherits A{
	z:Bool;
	func(a:String,b:Bool):SELF_TYPE{
		self@A.func(a,b)
	};
};

Class A{
	x:Bool;
	y:String;
	func(x:String,y:Bool):SELF_TYPE{
		self
	};
};
