(defpackage velopcs/tests/main
  (:use :cl
        :velopcs
        :rove))
(in-package :velopcs/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :velopcs)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
