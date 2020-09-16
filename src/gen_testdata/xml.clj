(ns gen-testdata.xml
  (:require [clojure.string :as cstr]
            [clojure.xml :as xml]))


(defn print-xml [x]
  (println (-> (with-out-str
                 (xml/emit-element x))
               (cstr/replace #"\r?\n" "")
               (cstr/replace #"'" "\""))))
