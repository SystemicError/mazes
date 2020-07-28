(ns mazes.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

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
         ;dummy (println (str "\nqueue: " queue
         ;                    "\nstack: " stack))
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

(defn generate-3d-maze
  "Generate a 3-dimensional rectangular maze."
  ([rows cols planes] (generate-3d-maze rows cols planes (list {:row 0 :col 0 :plane 0}) (list {:row 0 :col 0 :plane 0})))
  ([rows cols planes queue stack]
   (let [neighbors (fn [r c p] 
                     (list 
                       {:row (inc r) :col c :plane p}
                       {:row (dec r) :col c :plane p}
                       {:row r :col (inc c) :plane p}
                       {:row r :col (dec c) :plane p}
                       {:row r :col c :plane (inc p)}
                       {:row r :col c :plane (dec p)}
                       ))
         inbounds (fn [r c p]
                    (filter #(and (<= 0 (:row %))
                                  (<= 0 (:col %))
                                  (<= 0 (:plane %))
                                  (< (:row %) rows)
                                  (< (:col %) cols)
                                  (< (:plane %) planes)
                                  )
                            (neighbors r c p)))
         unvisited (fn [r c p path]
                     (filter #(not ((set path) %)) (inbounds r c p)))
         ;dummy (println (str "\nqueue: " queue
         ;                    "\nstack: " stack))
         ]
     (if (empty? stack)
       (let [q (first queue)
             queue-neighbors (unvisited (:row q) (:col q) (:plane q) queue)
             f (if (empty? queue-neighbors)
                 nil
                 (rand-nth queue-neighbors))
             ]
         (if (empty? queue-neighbors)
           ; finished, return queue
           queue
           ; push one neighbor onto stack and queue and recur
           (generate-3d-maze rows cols (cons f queue) (cons f stack))))
       (let [s (first stack)
             stack-neighbors (unvisited (:row s) (:col s) (:plane s) (concat stack queue))
             f (if (empty? stack-neighbors)
                 nil
                 (rand-nth stack-neighbors))
             ]
         (if (empty? stack-neighbors)
           ; pop one off stack into queue and recur
           (generate-3d-maze rows cols planes
                             (cons (first stack) queue)
                             (rest stack))
           ; push one neighbor onto stack and queue and recur
           (generate-3d-maze rows cols planes (cons f queue) (cons f stack))))))))

(defn path-2d-to-walls
  "Take in a 2d path and return a map of rooms with NESW door associations."
  ([rows cols path] (path-2d-to-walls rows cols path
                                      (apply hash-map
                                        (interleave
                                          (for [r (range rows)
                                                c (range cols)
                                                ]
                                            {:row r :col c})
                                          (for [r (range rows)
                                                c (range cols)
                                                ]
                                            (set nil))))))
  ([rows cols path grid]
   (if (or (empty? path)
           (= (count path) 1))
     grid ; base case
     (let [f (first path)
           s (first (rest path))
           square (fn [x] (* x x))
           distance (+ (square (- (:row f) (:row s)))
                       (square (- (:col f) (:col s))))
           direction (if (< (:row s) (:row f))
                       :north
                       (if (> (:row s) (:row f))
                         :south
                         (if (< (:col s) (:col f))
                           :west
                           :east)))
           antipode (case direction :north :south :south :north :east :west :west :east)
           ;dummy (println (str "Direction:" direction
           ;                    "=?" (= distance 1)
           ;                    "\nDistance: " distance))
           ]
       (recur rows cols
              (rest path)
              (if (= distance 1)
                (assoc grid
                       f (set (cons direction (grid f)))
                       s (set (cons antipode (grid s))))
                grid))))))

(defn path-3d-to-walls
  "Take in a 3d path and return a map of rooms with NESW/Up/Down door associations."
  ([rows cols planes path] (path-3d-to-walls rows cols planes path
                                      (apply hash-map
                                        (interleave
                                          (for [r (range rows)
                                                c (range cols)
                                                p (range planes)
                                                ]
                                            {:row r :col c :plane p})
                                          (for [r (range rows)
                                                c (range cols)
                                                p (range planes)
                                                ]
                                            (set nil))))))
  ([rows cols planes path grid]
   (if (or (empty? path)
           (= (count path) 1))
     grid ; base case
     (let [f (first path)
           s (first (rest path))
           square (fn [x] (* x x))
           distance (+ (square (- (:row f) (:row s)))
                       (square (- (:col f) (:col s)))
                       (square (- (:plane f) (:plane s))))
           direction (if (< (:row s) (:row f))
                       :north
                       (if (> (:row s) (:row f))
                         :south
                         (if (< (:col s) (:col f))
                           :west
                           (if (> (:col s) (:col f))
                             :east
                             (if (< (:plane s) (:plane f))
                               :down
                               :up)))))
           antipode (case direction :north :south :south :north :east :west :west :east :up :down :down :up)
           ;dummy (println (str "Direction:" direction
           ;                    "=?" (= distance 1)
           ;                    "\nDistance: " distance))
           ]
       (recur rows cols planes
              (rest path)
              (if (= distance 1)
                (assoc grid
                       f (set (cons direction (grid f)))
                       s (set (cons antipode (grid s))))
                grid))))))


(defn maze-2d-to-string
  "Render a 2d maze nicely as a string."
  [maze]
  (let [rows (inc (apply max (map :row (keys maze))))
        cols (inc (apply max (map :col (keys maze))))
        ew (fn [cell] (if (cell :east)
                        (if (cell :west)
                          "-+-"
                          " +-")
                        (if (cell :west)
                          "-+ "
                          " + ")))
        n (fn [cell] (if (cell :north) " | " "   "))
        s (fn [cell] (if (cell :south) " | " "   "))
        row-to-string (fn [row]
                        (apply str
                               (str (apply str (map n row)) \newline )
                               (str (apply str (map ew row)) \newline )
                               (str (apply str (map s row)) \newline )))
        grid (for [r (range rows)]
               (for [c (range cols)]
                 (maze {:row r :col c})))
        ]
    (apply str (map row-to-string grid))
    ))

(defn draw-2d-maze
  "Draw a new 2d maze."
  [rows cols]
  (println (maze-2d-to-string (path-2d-to-walls rows cols (generate-2d-maze rows cols)))))



(def spacing 300)

;;; matrix functions

(defn rot-x
  [angle]
  "Return a rotation matrix about the x-axis."
  (let [c (Math/cos angle)
	s (Math/sin angle)
	]
    (list (list 1 0 0)
          (list 0 c (* -1 s))
          (list 0 s c))))

(defn rot-y
  [angle]
  "Return a rotation matrix about the y-axis."
  (let [c (Math/cos angle)
	s (Math/sin angle)
	]
    (list (list c 0 (* -1 s))
          (list 0 1 0)
          (list s 0 c))))

(defn rot-z
  [angle]
  "Return a rotation matrix about the x-axis."
  (let [c (Math/cos angle)
	s (Math/sin angle)
	]
    (list (list c (* -1 s) 0)
          (list s c 0)
	  (list 0 0 1))))

(defn matrix-multiply
  [a b]
  (for [row (range (count a))]
    (for [col (range (count (first b)))]
      (reduce + (map *
                     (nth a row)
                     (map #(nth % col) b))))))

;;; quil functions

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to (default) RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state.
  {:x (/ spacing 2)
   :y (/ spacing 2)
   :z (/ spacing 2)
   :azimuth 0.0
   :maze (path-3d-to-walls 6 6 6 (generate-3d-maze 6 6 6))
   })

(defn update-state [state]
  (let [x (:x state)
        y (:y state)
        z (:z state)
        azimuth (:azimuth state)
        pressed (q/key-as-keyword)
        dx (* 10 (Math/cos azimuth))
        dy (* 10 (Math/sin azimuth))
        new-x (+ x (if (= pressed :w) dx 0))
        new-y (+ y (if (= pressed :w) dy 0))
        dangle (case pressed
                 :d -0.04
                 :a 0.04
                 0.0)
        dz (case pressed
             :q 10
             :e -10
             0.0)
        ]
    (assoc state
           :azimuth (+ azimuth dangle)
           :x new-x
           :y new-y
           :z (+ z dz)
            )))

(defn draw-maze-walls
  "Draw the walls of a maze."
  [maze]
  (doseq [cell maze]
    (let [location (first cell)
          row (:row location)
          col (:col location)
          plane (:plane location)
          doors (first (rest cell))
          red (+ 128 (rem (* (rem row 4) 64) 128))
          green (+ 128 (rem (* (rem (inc row) 5) 154) 128))
          blue (+ 128 (rem (* (rem row 7) 64) 128))
          thickness 5
          ]
      (q/with-translation [(* col spacing) (* row spacing) (* plane spacing)]
        ; west
        (q/fill red green blue)
        (if (not (:west doors))
          (q/with-translation [(/ thickness 2) (/ spacing 2) (/ spacing 2)]
            (q/box thickness spacing spacing)))
        ; north
        (q/fill red blue green)
        (if (not (:north doors))
          (q/with-translation [(/ spacing 2) (/ thickness 2) (/ spacing 2)]
            (q/box spacing thickness spacing)))
        ; east
        (q/fill blue red green)
        (if (not (:east doors))
          (q/with-translation [(- spacing (/ thickness 2)) (/ spacing 2) (/ spacing 2)]
            (q/box thickness spacing spacing)))
        ; south
        (q/fill blue green red)
        (if (not (:south doors))
          (q/with-translation [(/ spacing 2) (- spacing (/ thickness 2)) (/ spacing 2)]
            (q/box spacing thickness spacing)))
        ; up 
        (q/fill green red blue)
        (if (not (:down doors))
          (q/with-translation [(/ spacing 2) (/ spacing 2) (/ thickness 2)]
            (q/box spacing spacing thickness)))
        ; down
        (q/fill green blue red)
        (if (not (:up doors))
          (q/with-translation [(/ spacing 2) (/ spacing 2) (- spacing (/ thickness 2))]
            (q/box spacing spacing thickness)))
        ))))

(defn draw-state [state]
  (let [x (:x state)
        y (:y state)
        z (:z state)
        azimuth (:azimuth state)
        look-x (+ x (* (Math/cos azimuth) 800))
        look-y (+ y (* (Math/sin azimuth) 800))
        look-z z
        ]
    ; Clear the sketch by filling it color
    (q/background 240)
    ; Set 3d mode
    (q/camera x y z
              look-x look-y look-z
              0 0 1)
    (q/perspective)
    (draw-maze-walls (:maze state))
    ))

(q/defsketch free-group
  :title "Mazes"
  :size [768 512]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :renderer :p3d
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

