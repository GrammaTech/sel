template <template <class> class> struct a;
template <template <class> class b> using c = typename a<b>::d;
