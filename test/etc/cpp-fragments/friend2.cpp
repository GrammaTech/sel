class A {
    template<typename T>
    friend class B; // every B<T> is a friend of A
 
    template<typename T>
    friend void f(T) {} // every f<T> is a friend of A
};
