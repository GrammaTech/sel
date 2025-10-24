template <T> class GenericHolder<T> { T x; };

typedef GenericHolder<int> int_holder;

class InheritsFromTypedef : int_holder {};
