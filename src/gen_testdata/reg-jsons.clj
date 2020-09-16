(ns gen-testdata.reg-jsons
  (:import com.github.javafaker.Faker
           java.util.concurrent.TimeUnit
           java.text.SimpleDateFormat)
  (:require [clojure.data.json :as json]
            [clojure.java.io :refer [file]])
  (:gen-class))

;; =============
;; Generate data
;; =============

(def faker (new Faker))

(defn get-rand-line [file-name]
  (->> file-name
       clojure.java.io/resource
       slurp
       clojure.string/split-lines
       rand-nth))

(def gen-group (constantly "Знания для всех, даром"))
(def gen-active (constantly "true"))
(def gen-credentials-free #(let [login (.. faker name username)
                                 password (.. faker internet password)]
                             [login password]))
(def gen-credentials-pin (let [i (atom 24)] #(let [pin (format "%012d" (swap! i inc))] [pin pin])))
(def gen-credentials-repetitor (let [i (atom 0)] #(let [i (swap! i inc)] (map str ["user" "pass"] (repeat i)))))
(def gen-family-name #(get-rand-line "surnames.txt"))
(def gen-first-name #(get-rand-line "names.txt"))
(def gen-second-name #(get-rand-line "second_names.txt"))
(def gen-email #(.. faker internet emailAddress))
(def gen-organization #(.. faker company name))
(def gen-sex (constantly "М"))
(def gen-birthday #(. (new SimpleDateFormat "dd.MM.yyyy") format (.. faker date (past 36500 TimeUnit/DAYS))))
(def gen-phone #(.. faker phoneNumber phoneNumber))
(def gen-partner-code #(.. faker code isbn10))
(def gen-city #(get-rand-line "cities.txt"))
(def gen-delivery-order #(rand-nth [:post :distributor :by-myself]))
(def gen-postcode #(.. faker address zipCode))
(def gen-delivery-address #(.. faker address streetAddress))
(def gen-distributor #(get-rand-line "distributors.txt"))
(def gen-source (constantly "От партнера"))
(def gen-exam-theme #(get-rand-line "exam_themes.txt"))
(def gen-passport-series #(.. faker number (randomNumber 4 false) toString))
(def gen-passport-number #(.. faker number (randomNumber 10 false) toString))
(def gen-repetitor-themes (constantly [{:id 1,
                                        :lectures true,
                                        :autoCheckTests true,
                                        :manualCheckTests true}
                                       {:id 2,
                                        :lectures true,
                                        :autoCheckTests true,
                                        :manualCheckTests false}
                                       {:id 3,
                                        :lectures true,
                                        :autoCheckTests false,
                                        :manualCheckTests true}]))

(defn post-process [data]
  (let [add-delivery-data (fn [data] (if-let [delivery-order (data :delivery_order)]
                                       (merge data
                                              (case delivery-order
                                                :post {:delivery_order "По почте"
                                                       :delivery_city (gen-city)
                                                       :postcode (gen-postcode)
                                                       :address (gen-delivery-address)}
                                                :distributor {:delivery_order "Через дистрибьютора"
                                                              :distributor (gen-distributor)}
                                                :by-myself {:delivery_order "Заберу сам(а)"}))
                                       data))
        add-credentials (fn [data] (let [[login password] (data :credentials)]
                                     (merge (dissoc data :credentials)
                                            {:login login :password password})))]
    (->> data
         add-delivery-data
         add-credentials)))

(defn gen-data
  "Generate data structure depends on given hash-map of functions"
  [gens]
  (let [data (into {} (map (fn [[k v]] [k (v)]) gens))]
    (post-process data)))

;; =======
;; Courses
;; =======

(def courses
  (let [describe-course (fn [additional-fields]
                          (merge additional-fields {:group gen-group
                                                    :familyName gen-family-name
                                                    :firstName gen-first-name
                                                    :secondName gen-second-name
                                                    :email gen-email
                                                    :sex gen-sex
                                                    :city gen-city}))]
    {:basic {:group gen-group
             :familyName gen-family-name
             :firstName gen-first-name
             :secondName gen-second-name
             :email gen-email
             :active gen-active
             :credentials gen-credentials-free}
     :free (describe-course {:credentials gen-credentials-free})
     :course (describe-course {:credentials gen-credentials-pin
                               :organization gen-organization ;; optional
                               :birthday gen-birthday ;; optional
                               :phone gen-phone ;; optional
                               :partner_code gen-partner-code ;; optional
                               :delivery_order gen-delivery-order})
     :exam (describe-course {:credentials gen-credentials-pin
                             :organization gen-organization ;; optional
                             :birthday gen-birthday ;; optional
                             :phone gen-phone ;; optional
                             :source gen-source
                             :partner_code gen-partner-code
                             :delivery_order gen-delivery-order
                             :exam_theme gen-exam-theme
                             :passport_series gen-passport-series
                             :passport_number gen-passport-number})
     :training (describe-course {:credentials gen-credentials-pin
                                 :organization gen-organization ;; optional
                                 :birthday gen-birthday ;; optional
                                 :phone gen-phone ;; optional
                                 :source gen-source})
     :repetitor  {:credentials gen-credentials-repetitor
                  :familyName gen-family-name
                  :firstName gen-first-name
                  :secondName gen-second-name
                  :email gen-email
                  :emailParent gen-email
                  :active (constantly true)
                  :paidTestCount (constantly 2)
                  :themes gen-repetitor-themes}}))

;; ===================
;; Generate json files
;; ===================

(defn gen-and-write
  [gens path]
  (let [data (gen-data gens)
        json-str (with-out-str (json/pprint data :escape-unicode false))]
    (spit path json-str)))

(defn gen-json-files [course-key amount folder-path]
  (let [gens (courses course-key)
        get-path #(.getAbsolutePath (file (str folder-path (name course-key) "_" % ".json")))]
    (doseq [i (range 1 (inc amount))
            :let [json-path (get-path i)]]
      (gen-and-write gens json-path)
      (println json-path))))

(defn parse-cli-args [args]
  (let [hint-and-exit (fn [] (let [example (clojure.string/join " "
                                                                (map #(str (name %1) " " %2)
                                                                     (keys courses)
                                                                     (repeatedly #(rand-nth (range 1 10)))))]
                               (println "Args: <course-type> <amount> [...] Example:" example)))
        pairs (map (fn [[course-name amount]] [(keyword course-name) amount])
                   (partition-all 2 (map read-string args)))]
    (or (and (seq pairs)
             (every? courses (map first pairs))
             (every? integer? (map second pairs))
             pairs)
        (hint-and-exit))))

(defn -main [& args]
  (doseq [[course-key amount] (parse-cli-args args)]
    (gen-json-files course-key amount "")))
