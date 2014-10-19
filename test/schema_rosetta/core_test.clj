(ns schema-rosetta.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [check]]
            [schema-rosetta.core :refer [human-check]]))

(deftest class-explainer
  (let [chk (human-check s/Str)]
    (is (nil? (chk "foo")))
    (is (= (chk 28) "28 is not a java.lang.String"))))

(deftest pred-explainer
  (let [chk (human-check s/Int)]
    (is (nil? (chk 28)))
    (is (= (chk 0.5) "0.5 is not integer"))
    (is (= (chk "foo") "foo is not integer")))

  ;; same for s/Keyword
  (let [chk (human-check s/Keyword)]
    (is (nil? (chk :yolo)))
    (is (= (chk "yolo") "yolo is not keyword")))

  ;; TODO: this with more interesting predicates
  (let [chk (human-check (s/pred #(even? %)
                                  'even?))]
    (is (nil? (chk 2)))
    (is (= "1 is not even" (chk 1)))))

(deftest eq-explainer
  (let [chk (human-check (s/eq 42))]
    (is (nil? (chk 42)))    
    (is (= (chk 43) "43 is not eq with 42"))))

(deftest map-explainer
  (let [chk (human-check {:foo s/Str, :bar s/Str})]
    (is (nil? (chk {:foo "foo", :bar "bar"})))
    (is (= (chk {:foo 42, :bar "bar"})
           {:foo "42 is not a java.lang.String"}))
    (is (= (chk {:foo "foo", :bar 42})
           {:bar "42 is not a java.lang.String"}))))

(deftest enum-explainer
  (let [chk (human-check (s/enum 2 3 4))]
    (is (nil? (chk 2)))
    (is (= (chk 5) "5 is not one of #{4 3 2}"))))

(s/defschema Nested
  {:name s/Str
   :info {:email s/Str}})

(deftest nested-schema
  (let [chk (human-check Nested)]
    (is (nil? (chk {:name "funk d'void"
                    :info {:email "void@mixcloud.com"}})))
    (is (= {:info {:email "24 is not a java.lang.String"}
            :name "42 is not a java.lang.String"}
           (chk {:name 42 :info {:email 24}})))))
