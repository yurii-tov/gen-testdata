(ns gen-testdata.user-import
  (:require
   [clojure.string :refer [split-lines join]]
   [clojure.java.io :as io])
  (:import java.util.UUID))


(def letters (slurp (io/resource "letters.txt")))


(def header
  ["Параллель"
   "Класс"
   "Фамилия"
   "Имя"
   "Отчество"
   "Роль"
   "Пароль"
   "Дата рождения"])


(def names
  (zipmap [:first-names :second-names :surnames]
          (map (comp split-lines slurp io/resource)
               ["names.txt"
                "second_names.txt"
                "surnames.txt"])))


(def password-generators
  {:empty (constantly "")
   :constant (constantly "secret")
   :random #(subs (str (UUID/randomUUID)) 0 4)})


(def ^:dynamic *password-type* :empty)


(declare gen-name gen-user gen-group)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defn gen-users
  "Args: hashmap with keys:
  parallels - count of parallels (default is 1)
  groups    - count of groups within parallel (default is 1)
  students  - count of students in group (default is 0)
  teachers  - count of teachers in group (default is 0)
  admins    - count of admins in group (default is 0)
  parents   - count of parents in group (default is 0)
  partners  - count of partners in group (default is 0)
  password-type - type of password, a keyword (:empty, :constant or :random)
                  (default is :empty)
  Number of generated users =
        (students + teachers + parents + admins + partners) X groups X parallels"
  [options]
  (let [defaults {:parallels 1
                  :groups 1
                  :students 0
                  :teachers 0
                  :admins 0
                  :parents 0
                  :partners 0
                  :password-type :empty}
        {:keys [parallels groups password-type] :as options} (merge defaults options)]
    (binding [*password-type* password-type]
      (vec (for [parallel (range 1 (inc parallels))
                 letter (take groups letters)
                 group (gen-group parallel
                                  letter
                                  options)]
             group)))))


(defn print-csv
  "Args: hashmap as for gen-users fn (see gen-users documentation for details)"
  [options]
  (let [print-record (comp println (partial join ";"))]
    (print-record header)
    (doseq [record (gen-users options)]
      (print-record record))))


(defn write-csv
  "Args: hashmap as for gen-users fn (see gen-users documentation for details)"
  [options]
  (let [f (io/file (System/getenv "tmp") "output.csv")]
    (with-open [w (io/writer f :encoding "windows-1251")]
      (binding [*out* w]
        (print-csv options)))
    (printf "Write csv: %s\n"
            (.getAbsolutePath f))))


;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;


(defn gen-name []
  (map (comp rand-nth names)
       [:surnames :first-names :second-names]))


(defn gen-user
  [role]
  (concat (gen-name)
          [role
           ((password-generators *password-type*))
           "13.04.1985"]))


(defn gen-group
  [parallel
   letter
   {:keys [teachers
           students
           admins
           parents
           partners]}]
  (let [append-group-info
        (partial concat [(str parallel) (str parallel letter)])
        gen (comp append-group-info gen-user)]
    (mapcat
     (fn [c r] (take c (repeatedly #(gen r))))
     [teachers students parents admins partners]
     ["Преподаватель" "Учащийся" "Родитель" "Администратор" "Партнер"])))
