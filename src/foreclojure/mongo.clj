(ns foreclojure.mongo
  (:use somnium.congomongo
        [foreclojure.data-set :only [load-problems]]
        [foreclojure.config :only [config]]
        [foreclojure.problems :only [total-solved get-problem-list]]
        [foreclojure.users :only [get-users]]))

(defn connect-to-db []
  (let [{:keys [db-user db-pwd db-host]} config]
    (mongo!
     :host (or db-host "localhost")
     :db "mydb")
    (when (and db-user db-pwd)
      (authenticate db-user db-pwd))))

(defn prepare-problems []
  (when-not (fetch-one :problems)
    (load-problems))
  (add-index! :problems [:solved]))

(defn prepare-seqs []
  (update! :seqs
           {:_id "problems"}
           {:$set {:seq (inc (apply max
                                    (map :_id
                                         (fetch :problems
                                                :only [:_id]))))}}))

;; make it easier to get off the ground by marking contributors automatically
;; useful since some "in-development" features aren't enabled for all users
(defn prepare-users []
  (add-index! :users [:user] :unique true)
  (add-index! :users [[:solved -1]])
  (update! :users
           {:user {:$in ["amalloy" "dbyrne" "raynes" "cmeier" "devn"
                         "citizen428" "daviddavis" "clinteger"]}}
           {:$set {:contributor true}}
           :upsert false
           :multiple true))

(defn prepare-solutions []
  (add-index! :solutions [:user :problem]))

(defn reconcile-solved-count
  "Overwrites the times-solved field in the problems collection based on data from the users collection. Should only be called on server startup since it isn't a safe operation. Also updates the total-solved agent."
  []
  (send
   total-solved +
   (let [problems (get-problem-list)]
     (reduce
      #(do
         (update! :problems
                  {:_id (first %2)}
                  {:$set {:times-solved (last %2)}})
         (+ %1 (last %2)))
      0
      (reduce #(update-in %1 [%2] inc)
              (reduce #(conj %1 [%2 0])
                      {}
                      (map :_id problems))
              (mapcat :solved (get-users)))))))

(defn prepare-mongo []
  (connect-to-db)
  (prepare-problems)
  (prepare-seqs)
  (prepare-users)
  (prepare-solutions)
  (reconcile-solved-count))
