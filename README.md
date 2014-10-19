# Schema Rosetta

A custom `prismatic/schema` walker that provides human-friendly validation error
messages.

## Meet Rosetta

This is alpha software.

```clojure
(ns example.project
    (:require [schema.core :as s]
              [schema-rosetta.core :as rosetta]))

(s/defschema Person
  {:name s/Str
   :last-name s/Str
   :age (s/pred #(> % 21) 'greater-than-21)})

(defn validate-person
  [person]
  (let [checker (rosetta/human-check Person)]
    (checker person)))

```

## Using it

```clj
(defproject foo "0.1.0"
  ...
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.3.0"]
                 [datil/schema-rosetta "0.1.0-SNAPSHOT"]]
  ...)
```

## Caveats

At the moment, if disallowed keys are provided as an input to `human-check` only
those keys will be displayed as errors.

## License

Copyright (C) 2014 Datil and Contributors.  Distributed under the Eclipse Public License, the same as Clojure.
