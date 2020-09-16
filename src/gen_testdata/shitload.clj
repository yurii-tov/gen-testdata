(ns gen-testdata.shitload
  "Generate piles of XML to import into 1C:Education"
  (:require [gen-testdata.user-import :refer [names]]
            [gen-testdata.xml :refer [print-xml]])
  (:import java.util.UUID))


(defn gen-guid []
  (str (UUID/randomUUID)))


(defn gen-fname []
  (rand-nth (names :first-names)))


(defn gen-sname []
  (rand-nth (names :second-names)))


(defn gen-lname []
  (rand-nth (names :surnames)))


(defn gen-group [& {:keys [students-count
                           class-id]}]
  (let [class-id (or class-id (gen-guid))
        presence-period-start "2020-06-01"
        teacher {:tag :user
                 :attrs {:gender "М"
                         :birthdate ""
                         :surname (gen-lname)
                         :secondname (gen-sname)
                         :firstname (gen-fname)
                         :password ""
                         :id (gen-guid)}
                 :content [{:tag :role
                            :attrs {}
                            :content ["Преподаватель"]}
                           {:tag :class :attrs {:id class-id}
                            :content [{:tag :presence_period
                                       :attrs {:date_start presence-period-start
                                               :date_end ""}
                                       :content nil}]}]}
        gen-student (fn []
                      {:tag :user
                       :attrs {:gender "М"
                               :birthdate ""
                               :surname (gen-lname)
                               :secondname ""
                               :firstname (gen-fname)
                               :password ""
                               :id (gen-guid)}
                       :content [{:tag :role
                                  :attrs {}
                                  :content ["Учащийся"]}
                                 {:tag :class :attrs {:id class-id}
                                  :content [{:tag :presence_period
                                             :attrs {:date_start presence-period-start
                                                     :date_end ""}
                                             :content nil}]}]})]
    (cons teacher (take students-count (repeatedly gen-student)))))


(defn gen-journal [& {:keys [class-id
                             term-id
                             subject-id
                             tutor-id]}]
  {:tag :journal
   :attrs {:tutor_id tutor-id
           :subject_id subject-id
           :class_id class-id
           :term_id term-id
           :id (gen-guid)
           :markscale_id "507f4400-327f-11e3-aa6e-0800200c9a66"}
   :content nil})
