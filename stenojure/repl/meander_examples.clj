(require '[meander.epsilon :as m])

(def people [{:name           :alice
              :favorite-foods [:nachos]}
             {:name           :mary
              :favorite-foods [:pizza]}
             {:name           :bob
              :favorite-foods [:nachos :pizza]}])


(= (->> people
        (filter #(= 1 (count (:favorite-foods %)))))
   (-> people
       (m/search
         (m/scan {:name           ?name
                  :favorite-foods [?food]})
         {:name           ?name
          :favorite-foods [?food]})))


(= (->> people
        (filter #(= 1 (count (:favorite-foods %))))
        (mapv #(hash-map :name (:name %)
                         :favorite-food (first (:favorite-foods %)))))
   (-> people
       (m/search
         (m/scan {:name           ?name
                  :favorite-foods [?food]})
         {:name          ?name
          :favorite-food ?food})))

(defn group-by-word
  ""
  [ms]
  (-> (group-by :word ms)
      (m/search
        (m/scan [?id [{:strokes !s} ...]])
        {:word    ?id
         :strokes !s})))