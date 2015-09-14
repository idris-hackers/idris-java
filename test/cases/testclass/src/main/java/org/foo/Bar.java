package org.foo;

public class Bar {
    Foo foo;
    
    public Bar(String str, int i) {
	foo = new Foo();
    }
    
    public void dodynamic() {
	foo.foodynamic("Bar calling Foo dynamic");
    }
}
