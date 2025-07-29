(ns depysible.salience-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [depysible.core :refer [parse-rule parse-program]]
            [depysible.domain.definitions :refer [rule-types]]))

(deftest test-salience-parsing
  (testing "Parse defeasible rule with salience"
    (let [rule (parse-rule "flies(X) -< bird(X) [5].")]
      (is (= (:salience rule) 5))
      (is (= (:type rule) (:defeasible rule-types)))
      (is (= (str (:head rule)) "flies(X)"))
      (is (= (count (:body rule)) 1))))

  (testing "Parse strict rule with salience"
    (let [rule (parse-rule "bird(X) <- chicken(X) [3].")]
      (is (= (:salience rule) 3))
      (is (= (:type rule) (:strict rule-types)))
      (is (= (str (:head rule)) "bird(X)"))
      (is (= (count (:body rule)) 1))))

  (testing "Parse fact with salience"
    (let [rule (parse-rule "bird(tweety) [7].")]
      (is (= (:salience rule) 7))
      (is (= (:type rule) (:strict rule-types)))
      (is (= (str (:head rule)) "bird(tweety)"))
      (is (= (count (:body rule)) 0))))

  (testing "Parse rule without salience defaults to 0"
    (let [rule (parse-rule "flies(X) -< bird(X).")]
      (is (= (:salience rule) 0))
      (is (= (:type rule) (:defeasible rule-types)))))

  (testing "Parse multiple rules with different salience"
    (let [program (parse-program "flies(X) -< bird(X) [1].
                                  ~flies(X) -< penguin(X) [5].
                                  bird(X) <- chicken(X) [2].")
          rules (:rules program)]
      (is (= (count rules) 3))
      (is (= (:salience (first rules)) 1))
      (is (= (:salience (second rules)) 5))
      (is (= (:salience (nth rules 2)) 2))))

  (testing "Parse complex rule with body and salience"
    (let [rule (parse-rule "flies(X) -< bird(X), ~penguin(X), alive(X) [10].")]
      (is (= (:salience rule) 10))
      (is (= (count (:body rule)) 3))
      (is (= (:type rule) (:defeasible rule-types)))))

  (testing "Negative salience values"
    (let [rule (parse-rule "low_priority(X) -< condition(X) [-1].")]
      (is (= (:salience rule) -1))
      (is (= (:type rule) (:defeasible rule-types)))))

  (testing "Large salience values"
    (let [rule (parse-rule "critical(X) -< emergency(X) [999].")]
      (is (= (:salience rule) 999))
      (is (= (:type rule) (:defeasible rule-types)))))) 