(ns mazes.core)

; queue-stack-method:
;   if stack empty:
;     get list of unvisisted neigbhors to end of queue
;     if none:
;       finished
;     else:
;       push one queue-neighbor onto stack and onto queue and recur
;   else
;     get list of neighbors to end of stack
;     if none:
;       pop one off stack and enqueue result
;       recur
;     else:
;       push one stack-neighbor onto stack and onto queue and recur

(defn generate-2d-maze
  "Generate a 2-dimensional rectangular maze."
  ([rows cols] (generate-2d-maze rows cols (list {:row 0 :col 0}) (list {:row 0 :col 0})))
  ([rows cols queue stack]
   (let [neighbors (fn [r c] 
                     (list 
                       {:row (inc r) :col c}
                       {:row (dec r) :col c}
                       {:row r :col (inc c)}
                       {:row r :col (dec c)}))
         inbounds (fn [r c]
                    (filter #(and (<= 0 (:row %))
                                  (<= 0 (:col %))
                                  (< (:row %) rows)
                                  (< (:col %) cols))
                            (neighbors r c)))
         unvisited (fn [r c path]
                     (filter #(not ((set path) %)) (inbounds r c)))
         dummy (println (str "\nqueue: " queue
                             "\nstack: " stack))
         ]
     (if (empty? stack)
       (let [q (first queue)
             queue-neighbors (unvisited (:row q) (:col q) queue)
             f (if (empty? queue-neighbors)
                 nil
                 (rand-nth queue-neighbors))
             ]
         (if (empty? queue-neighbors)
           ; finished, return queue
           queue
           ; push one neighbor onto stack and queue and recur
           (generate-2d-maze rows cols (cons f queue) (cons f stack))))
       (let [s (first stack)
             stack-neighbors (unvisited (:row s) (:col s) (concat stack queue))
             f (if (empty? stack-neighbors)
                 nil
                 (rand-nth stack-neighbors))
             ]
         (if (empty? stack-neighbors)
           ; pop one off stack into queue and recur
           (generate-2d-maze rows cols
                             (cons (first stack) queue)
                             (rest stack))
           ; push one neighbor onto stack and queue and recur
           (generate-2d-maze rows cols (cons f queue) (cons f stack))))))))
