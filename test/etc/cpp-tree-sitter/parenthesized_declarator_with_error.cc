class ReturnAction {
  template <typename R_, typename F> class Impl : public ActionInterface<F> {
    GTEST_COMPILE_ASSERT_(!std::is_reference<Result>::value,
                          Result_cannot_be_a_reference_type);
  };
};
