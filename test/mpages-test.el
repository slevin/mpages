(ert-deftest mpages-test-formatting ()
  "Test time formatting"
  (should (string= (mpages-tfmt '(0 61 0 0)) "01:01")))
