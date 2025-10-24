template <T> class GenericHolder<T> { T x; };

using int_holder = GenericHolder<int>;

class InheritsFromAlias : int_holder {};
