(ns depysible.rete-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [depysible.domain.definitions :refer [->LogicAtom create-atom atom-unifies]]))

(deftest test-atom-unification
  (testing "unifies with empty terms"
    (let [pattern (->LogicAtom "x" [])
          ground (->LogicAtom "x" [])]
      (is (= {} (atom-unifies pattern ground)))))

  (testing "fails to unify different functors"
    (let [pattern (->LogicAtom "x" [])
          ground (->LogicAtom "y" [])]
      (is (nil? (atom-unifies pattern ground)))))

  (testing "unifies with same constants"
    (let [pattern (->LogicAtom "x" [5])
          ground (->LogicAtom "x" [5])]
      (is (= {} (atom-unifies pattern ground)))))

  (testing "fails to unify different functors with different terms"
    (let [pattern (->LogicAtom "x" ["b"])
          ground (->LogicAtom "y" [5])]
      (is (nil? (atom-unifies pattern ground)))))

  (testing "unifies variable with constant"
    (let [pattern (->LogicAtom "x" ["X"])
          ground (->LogicAtom "x" [5])]
      (is (= {"X" 5} (atom-unifies pattern ground)))))

  (testing "fails to unify same variable with different values"
    (let [pattern (->LogicAtom "x" ["X" "X"])
          ground (->LogicAtom "y" [5 7])]
      (is (nil? (atom-unifies pattern ground)))))

  (testing "unifies multiple variables with different values"
    (let [pattern (->LogicAtom "x" ["X" "Y"])
          ground (->LogicAtom "x" [5 "b"])]
      (is (= {"X" 5 "Y" "b"} (atom-unifies pattern ground))))))

(deftest test-complex-unification
  (testing "complex unification scenarios"
    (let [pattern (->LogicAtom "pred" ["X" "Y" "X"])
          ground (->LogicAtom "pred" [1 2 1])]
      (is (= {"X" 1 "Y" 2} (atom-unifies pattern ground))))
    
    (let [pattern (->LogicAtom "pred" ["X" "Y" "X"])
          ground (->LogicAtom "pred" [1 2 3])]
      (is (nil? (atom-unifies pattern ground)))))) 