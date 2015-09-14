package org.foo;

public class Foo
{
    public int field;
        
    public Foo() {
	field = 10;
	System.out.println("In Foo Constructor");
    }

    public void foodynamic(String str) {
	System.out.println("field = " + field);
	System.out.println("str = " + str);
    }
}
