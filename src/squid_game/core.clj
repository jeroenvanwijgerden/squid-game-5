(ns squid-game.core
  (:require [oz.core :as oz]))


(defn end-states
  [n-players n-panes]
  (->> (let [end-state? (fn [alive pane]
                          (or (empty? alive)
                              (= pane n-panes)))
             players (range 1 (inc n-players))
             p-success 1/2]
         (loop [end-states []
                states [[1 players 0 []]]]
           (if-let [[p alive pane player+died-at-pane :as state] (peek states)]
             (if (end-state? alive pane)
               (recur (conj end-states
                            state)
                      (pop states))
               (recur end-states
                      (conj (pop states)
                            [(* p p-success)
                             alive
                             (inc pane)
                             player+died-at-pane]
                            [(* p (- 1 p-success))
                             (rest alive)
                             (inc pane)
                             (conj player+died-at-pane
                                   [(first alive)
                                    (inc pane)])])))
             end-states)))))


(defn end-state->player+pane+p
  [[p alive final-pane player+died-at-pane]]
  (let [player+pane-last-alive (concat (for [player alive]
                                         [player final-pane])
                                       (for [[player died-at-pane] player+died-at-pane]
                                         [player (dec died-at-pane)]))]
    (mapcat (fn [[player pane-last-alive]]
              (for [pane (range 1 (inc pane-last-alive))]
                [player pane p]))
            player+pane-last-alive)))


(defn aggregate-ps
  [player+pane+p]
  (->> player+pane+p
       (group-by (fn [[player pane]]
                   [player pane]))
       (map (fn [[[player pane] player+pane+p]]
              [player pane (reduce + (for [[_ _ p] player+pane+p] p))]))))


(defn plot-values
  [player+pane+p]
  (map (fn [[player pane p]]
         {:player (if (< player 10)
                    (str "0" player)
                    (str     player))
          :pane pane
          :p (double p)})
       player+pane+p))


(defn plot [values]
  {:data {:values values}
   :encoding {:x {:field "pane"
                  :title "pane"
                  :type "ordinal"}
              :y {:field "p"
                  :title "p"
                  :type "quantitative"}
              :color {:field "player" :type "nominal"}}
   :mark "line"})


(comment (oz/start-server!)

         
         (def aggregated-ps (->> (end-states 16 18)
                                 (mapcat end-state->player+pane+p)
                                 aggregate-ps
                                 time))

         
         (->> aggregated-ps
              (keep (fn [[player pane p]]
                      (when (= pane 18)
                        [player (double p)])))
              sort)

         
         (oz/view! (->> aggregate-ps
                        plot-values
                        plot))
         )