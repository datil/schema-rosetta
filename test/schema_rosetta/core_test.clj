(ns schema-rosetta.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [check]]
            [schema-rosetta.core :refer [human-check
                                         human-walker-j]]))

(deftest class-explainer
  (let [chk (partial human-check s/Str)]
    (is (nil? (chk "foo")))
    (is (= (chk 28) "28 is not a java.lang.String"))))

(deftest pred-explainer
  (let [chk (partial human-check s/Int)]
    (is (nil? (chk 28)))
    (is (= (chk 0.5) "0.5 is not integer"))
    (is (= (chk "foo") "foo is not integer")))

  ;; same for s/Keyword
  (let [chk (partial human-check s/Keyword)]
    (is (nil? (chk :yolo)))
    (is (= (chk "yolo") "yolo is not keyword")))

  ;; TODO: this with more interesting predicates
  (let [chk (partial human-check (s/pred #(even? %)
                                  'even?))]
    (is (nil? (chk 2)))
    (is (= "1 is not even" (chk 1)))))

(deftest eq-explainer
  (let [chk (partial human-check (s/eq 42))]
    (is (nil? (chk 42)))    
    (is (= (chk 43) "43 is not eq with 42"))))

(deftest map-explainer
  (let [chk (partial human-check {:foo s/Str, :bar s/Str})]
    (is (nil? (chk {:foo "foo", :bar "bar"})))
    (is (= (chk {:foo 42, :bar "bar"})
           {:foo "42 is not a java.lang.String"}))
    (is (= (chk {:foo "foo", :bar 42})
           {:bar "42 is not a java.lang.String"}))))

(deftest enum-explainer
  (let [chk (partial human-check (s/enum 2 3 4))]
    (is (nil? (chk 2)))
    (is (= (chk 5) "5 is not one of #{4 3 2}"))))

(deftest maybe-explainer
  (let [chk (partial human-check (s/maybe s/Keyword))]
    (is (nil? (chk :a)))
    (is (nil? (chk nil)))
    (is (= (chk 5) "5 is not keyword"))))

(s/defschema Nested
  {:name s/Str
   :info {:email s/Str}})

(deftest vector-schema
  (let [chk (partial human-check [s/Int])]
    (is (= nil (chk [1 2 3])))
    (is (= ["one is not integer"] (chk ["one" 2])))))

(deftest nested-schema
  (let [chk (partial human-check Nested)]
    (is (nil? (chk {:name "funk d'void"
                    :info {:email "void@mixcloud.com"}})))
    (is (= {:info {:email "24 is not a java.lang.String"}
            :name "42 is not a java.lang.String"}
           (chk {:name 42 :info {:email 24}})))))

(deftest schema-optional-key
  (let [chk (partial human-check {:name s/Str
                            (s/optional-key :age) s/Int})]
    (is (nil? (chk {:name "funk d'void"})))
    (is (= {:age "28 is not integer"
            :name "82 is not a java.lang.String"}
           (chk {:name 82 :age "28"})))))

(deftest schema-disallowed-key
  (let [chk (partial human-check {:name s/Str
                                  :age s/Int})]
    (is (nil? (chk {:name "funk d'void" :age 28})))
    (is (= {:key "disallowed key"}
           (chk {:name 82 :key "28"})))))

(deftest both-explainer
  (let [chk (partial human-check (s/both s/Str
                                   (s/pred #(> (count %) 0) 'min-length)
                                   (s/pred #(<= (count %) 5) 'max-length)))]
    (is (nil? (chk "1")))
    (is (nil? (chk "12345")))
    (is (= " is not min length" (chk "")))
    (is (= "123456 is not max length" (chk "123456")))))
