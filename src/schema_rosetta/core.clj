;; Original source code taken from:
;; https://raw.githubusercontent.com/cddr/integrity/master/src/integrity/human.clj
;; The schema `check` function returns an object which is not really suitable
;; for displaying an error to an end-user. However, it usually contains enough
;; information to generate one. This library parses the return value of an invocation
;; of `check` and generates human readable error messages

(ns schema-rosetta.core
  (:require [schema.core :as s]
            [schema.utils :as utils]
            [taoensso.tower :as tower])
  (:import (schema.utils ValidationError)))

;; ### Support for internationalization

(def ^{:private true}
  dictionary
  "`dictionary` defines translations so that error messages in multiple
languages can be easily supported"
  {:dev-mode? true
   :fallback-locale :en
   :dictionary
   {:en {:schema-rosetta.core
         {:it           "it"
          :not-eq       "is not eq with"
          :not-one-of   "is not one of"
          :is           "is"
          :is-not       "is not"
          :and          "and"
          :fails-all    "fails all of the following:-"
          :is-not-a     "is not a"}}
    :es {:schema-rosetta.core
         {:it           "eso"
          :not-eq       "no es igual a"
          :not-one-of   "no está en la lista"
          :is           "es"
          :is-not       "no es"
          :and          "y"
          :fails-all    "no cumple con las siguientes condiciones:-"
          :is-not-a     "no es un"}}}})

(defn- tval [k]
  (tower/t :en dictionary k))

(defrecord ProcessedError [message value])

(defn filterm
  "Returns a filtered map"
  [pred m]
  (select-keys m (for [[k v] m :when (pred v)] k)))

(declare filter-result-coll)
(declare filter-result-map)
(declare filter-success)

(defn filter-error-vals [m k v]
  (if (coll? v)
    (let [filtered (filter-success v)]
      (if (empty? filtered)
        (dissoc m k)
        (assoc m k filtered)))
    (dissoc m k)))

(defn filter-result-map [res]
  (let [m (reduce-kv filter-error-vals (empty res) res)]
    (if (empty? m)
      nil
      m)))

(defn filter-result-coll [res]
  (let [c (map filter-success
               (filterv #(or (instance? ProcessedError %) (coll? %)) res))]
    (if (empty? (filter (comp not nil?) c))
      nil
      (into (empty res) c))))

(defn filter-success [res]
  (cond
    (instance? ProcessedError res) (:message res)
    (map? res) (filter-result-map res)
    (coll? res) (filter-result-coll res)
    :else nil))

;; Helpers
(defn- humanize
  "`humanize` takes a value and returns a human readable representation
of that value"
 [v]
  (if (symbol? v)
    (-> (str (name v))
        (clojure.string/replace "?" "")
        (clojure.string/replace "-" " "))
    v))

(defprotocol HumanExplain
  (human-explain [schema error] "Explain an error related to this schema"))

(defn human-walker
  [s]
  (let [walk (s/walker s)]
    (fn [x]
      (let [result (if (instance? ProcessedError x) x (walk x))]
        (human-explain s result)))))

(defn human-checker
  [input-schema]
  (comp filter-success (s/start-walker human-walker input-schema)))

(defn human-check
  [input-schema input-value]
  ((human-checker input-schema) input-value))

(defn human-walker-j
  [input-schema]
  (comp 
    identity
    (s/start-walker
    (fn [s]
      (let [walk (s/walker s)]
        (fn [x]
          (let [
                _ (print "\nSchema: " s
                         "\nX val: " x
                         "\nX class: " (class x)
                         "\nSchema class: " (class s))
                result (if (instance? ProcessedError x)
                         (walk (:value x))
                         ; x
                         (walk x)
                         )]
            (print "\nRaw result: " result
                   "\nResult: " (utils/error-val result)
                   "\nResult class: " (class (utils/error-val result))
                   "\nSchema: " s
                   "\nX val: " x
                   "\nIs result error: " (utils/error? result))
            ; (if (and (coll? result) (coll? (first result)))
            ;   (print "\n1st result: " (first result)
            ;          "\n2nd result: " (second (first result))))
            ; (if (= (class s) schema.core.Predicate)
            ;   (print "\nFail exp: "
            ;          (class (.-fail-explanation (utils/error-val result)))
            ;          "\nExpl delay: "
            ;          @(.-expectation-delay (utils/error-val result))))
            ; (if (= (class (utils/error-val result)) schema.utils.ValidationError)
            ;   (print "\nFail exp: "
            ;          (.-fail-explanation (utils/error-val result))
            ;          "\nExpl delay: "
            ;          @(.-expectation-delay (utils/error-val result))))
            (print "\n-----------------\n")
            ; (if (and (utils/error? result)
            ;          (map? (utils/error-val result)))
            ;   ; (map (fn [[s-key s-val]]
            ;   ;        (human-explain (s/map-entry s-key s-val) s-val))
            ;   ;      (utils/error-val result))
            ;   (do
            ;     (println "Result is ERROR: "
            ;              (map (fn [[s-key s-val]]
            ;                     (human-explain (s/map-entry s-key :not-valid) 'not-valid)
            ;                     s-val
            ;                     )
            ;                   (utils/error-val result)))
            ;     nil)
            ;   (human-explain s result))
            (let [r (human-explain s result)]
              (println "HUMAN RESULT: " r)
              r)
            ; result
            ))))
    input-schema)))

(extend-protocol HumanExplain
  java.lang.Class
  (human-explain [schema result]
    (if (utils/error? result)
      (ProcessedError.
        (with-out-str
          (print (.-value (utils/error-val result))
                 (tval ::is-not-a)
                 schema))
        (.-value (utils/error-val result)))
      result)))

(extend-protocol HumanExplain
  schema.core.Predicate
  (human-explain [schema result]
    ; (println "\n\nPREDICATE:\n" schema "\n\n" result)
    (if (utils/error? result)
      (let [err (utils/error-val result)
            [pred val] @(.-expectation-delay err)]
        (ProcessedError.
          (with-out-str
            (print (.-value err) (tval ::is-not)
                   ;(or (.-fail-explanation err) 'not)
                   (let [name (.-pred-name schema)]
                     (cond
                      (symbol? name) (humanize name)
                      (seq name) (str (humanize (first name))
                                      " "
                                      (apply str
                                             (interpose (str " " (tval ::and) " ")
                                                        (map humanize (rest name)))))))))
          result))
      result)))

(extend-protocol HumanExplain
  schema.core.EqSchema
  (human-explain [schema result]
    (if (utils/error? result)
      (let [err (utils/error-val result)]
        (ProcessedError.
          (with-out-str
            (print (.-value err) (tval ::not-eq) (.-v schema)))
          (.-value err))))))

(extend-protocol HumanExplain
  schema.core.EnumSchema
  (human-explain [schema result]
    (if (utils/error? result)
      (let [err (utils/error-val result)]
        (ProcessedError.
          (with-out-str
            (print (.-value err) (tval ::not-one-of)
                   (.-vs schema)))
          (.-value err))))))

(extend-protocol HumanExplain
  schema.core.MapEntry
  (human-explain [schema result]
    ; (println "MapEntry result: " result "\n"
    ;                    "Schema: " schema)
    (if (utils/error? result)
      (let [err (utils/error-val result)]
        (if-let [err-cause (second err)]
          {(first err) (ProcessedError. (humanize err-cause) result)}))
      (if-let [err (second result)]
        {(first result) err}))))

(defn map-uknown-keys [errs]
  (map (fn [[k v]]
         (human-explain (s/map-entry k (utils/error [k 'disallowed-key]))
                        (utils/error [k 'disallowed-key]))
         ; (println "MUK: " k ":" v)
         ; {k v}
         )
       (utils/error-val errs))
  ; (utils/error-val errs)
  )

(extend-protocol HumanExplain
  clojure.lang.PersistentArrayMap
  (human-explain [schema result]
    ; (println "PersistentArrayMap result: " result "\n"
    ;          "Schema: " schema)
    (let [m (into {}
                  (if (and (utils/error? result) (map? (utils/error-val result)))
                    ; (map-uknown-keys result)
                    ; (utils/error-val result)
                    (map (fn [[schema-key schema-val] res]
                            ; (println "PersistentArrayMap: " (class res) "\n"
                            ;          "Schema key: " schema-key "\n"
                            ;          "Schema val: " schema-val)
                            ; (if (utils/error? res)
                            ;   (println "PersistentArrayMap: " (utils/error-val res) "\n"
                            ;          "Schema key: " (get (utils/error-val res) 0) "\n"
                            ;          "Schema val: " (get (utils/error-val res) 1)))
                            (human-explain (s/map-entry schema-key schema-val)
                                           (utils/error [(key res) (val res)])))
                          schema (utils/error-val result))
                    (map (fn [[schema-key schema-val] res]
                            ; (println "PersistentArrayMap: " (class res) "\n"
                            ;          "Schema key: " schema-key "\n"
                            ;          "Schema val: " schema-val)
                            ; (if (utils/error? res)
                            ;   (println "PersistentArrayMap: " (utils/error-val res) "\n"
                            ;          "Schema key: " (get (utils/error-val res) 0) "\n"
                            ;          "Schema val: " (get (utils/error-val res) 1)))
                            (human-explain (s/map-entry schema-key schema-val) res))
                          schema result)
                    )
                  )]
      (if (empty? m)
        nil
        m))))

(extend-protocol HumanExplain
  clojure.lang.PersistentHashMap
  (human-explain [schema result]
    (let [m (into {}
                  (if (and (utils/error? result) (map? (utils/error-val result)))
                    (map (fn [[schema-key schema-val] res]
                           (human-explain (s/map-entry schema-key schema-val)
                                          (utils/error [(key res) (val res)])))
                         schema (utils/error-val result))
                    (map (fn [[schema-key schema-val] res]
                           (human-explain (s/map-entry schema-key schema-val) res))
                         schema result)))]
      (if (empty? m)
        nil
        m))))

(extend-protocol HumanExplain
  clojure.lang.PersistentVector
  (human-explain [schema result]
    (let [v (into [] (mapv (fn [schema-val res]
                             (human-explain schema-val res))
                           schema result))]
      (if (empty? v)
        nil
        v))))

(extend-protocol HumanExplain
  schema.core.Either
  (human-explain [schema result]
    result))

(extend-protocol HumanExplain
  schema.core.Both
  (human-explain [schema result]
    result))

(extend-protocol HumanExplain
  schema.core.Maybe
  (human-explain [schema result]
    result))
