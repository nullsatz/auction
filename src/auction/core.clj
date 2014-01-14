(ns auction.core
  (:gen-class))

(require '[clojure.set :as set])
(require '[clojure.pprint :as pretty])

(defn rand-weights [nbidders nitems]
  (apply hash-map
         (apply concat
                (for [i (range nbidders)
                      j (range nitems)]
                  [[i, j] (rand-int (* nbidders nitems))]))))

(defn benefits [weights prices]
  (apply hash-map
         (apply concat
                (map (fn [k]
                       (let [bidder (first k)
                             item (second k)]
                         [k  (- (get weights k 0)
                                (get prices item 0))]))
                     (keys weights)))))

(defn max-key-pair [m first-key]
  (let [m (select-keys m (filter #(= first-key (first %)) (keys m)))]
    (apply (partial max-key #(get m % 0)) (keys m))))

(defn max-value-items [benefits bidders]
  (map #(max-key-pair benefits %) bidders))

(defn bid-inc [values max-key second-max-key]
  (+ 1.0
     (- (get values max-key 0)
        (get values second-max-key 0))))

(defn place-bids [weights prices bidders items]
  (let [bidder-item-values (benefits weights prices)
        maxes (max-value-items bidder-item-values bidders)
        second-maxes (max-value-items
                      (apply (partial dissoc bidder-item-values)
                             maxes)
                      bidders)]
    (apply hash-map
           (interleave
            maxes
            (map (partial bid-inc bidder-item-values)
                 maxes second-maxes)))))

(defn max-bid [bids item]
  (let [item-bids (select-keys bids
                               (filter #(= item (second %))
                                       (keys bids)))]
    (if-not (empty? item-bids)
      (apply (partial max-key #(get item-bids % 0)) (keys item-bids)))))

(defn advance-wins [prev-wins bids items]
  (let [next-wins (filter #(if % %) (map #(max-bid bids %) items))
        next-items (set (map second next-wins))
        keeps (filter #(not (next-items (second %))) prev-wins)]
    (concat keeps next-wins)))

(defn decide-bidders [weights wins]
  (let [bidders (set (map first (keys weights)))]
    (set/difference bidders (set (map first wins)))))

(defn update-prices [prices bids wins]
  (let [new-prices (select-keys bids wins)
        new-prices (apply hash-map
                          (apply concat
                                 (map
                                  #(list (second %) (get new-prices %))
                                  (keys new-prices))))]
    (merge prices new-prices)))

(defn auction [weights]
  (let [items (set (map second (keys weights)))]
    (loop [bidders (set (map first (keys weights)))
           prices (apply hash-map
                         (interleave
                          items
                          (repeat (count items) 0)))
           wins []]
      (if (empty? bidders) wins
          (let [bids (place-bids weights prices bidders items)
                new-wins (advance-wins wins bids items)
                new-bidders (decide-bidders weights new-wins)
                new-prices (update-prices prices bids new-wins)]
            (recur new-bidders new-prices new-wins))))))

(defn -main
  "run a random auction"
  [& args]
  (let [w (rand-weights 5 5)
        a (auction w)]
    (println "weights")
    (pretty/pprint w)
    (println "a matching")
    (pretty/pprint a)))
