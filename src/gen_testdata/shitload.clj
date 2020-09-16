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
                           class-id
                           parallel
                           class-name]}]
  (let [class-id (or class-id (gen-guid))
        parallel (or parallel "1")
        class-name (or class-name (gen-guid))
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


(defn gen-journal [{:keys [class_id term_id subject_id tutor_id]}]
  (let [guid (gen-guid)]
    (format "<journal markscale_id=\"507f4400-327f-11e3-aa6e-0800200c9a66\" id=\"%s\" term_id=\"%s\" class_id=\"%s\" subject_id=\"%s\" tutor_id=\"%s\"/>"
            guid
            term_id
            class_id
            subject_id
            tutor_id)))
