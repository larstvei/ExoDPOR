(ns exogenous.visualize
  (:require [exogenous.core :as exo]
            [quil.core :as q]
            [clojure.string :refer [upper-case]]
            [quil.middleware :as m]
            [clojure.set :refer [difference]]
            [render-latex.core :as tex]))

(def rw-regex #"^p(\d+)_\d+_(r|w)\((\d+)\)$")

(def latex
  (memoize
   (fn [texstr]
     (let [svgfile (tex/latex->svg texstr)
           shp (q/load-shape svgfile)]
       (.disableStyle shp)
       shp))))

(def init-state {[] {}})

(defn setup [& maybe-state]
  (q/no-stroke)
  (q/stroke-weight 2)
  (q/rect-mode :center)
  (q/text-align :center)
  (q/shape-mode :center)
  (if maybe-state
    (first maybe-state)
    {:history [@exo/search-state]
     :pos 0
     :bg 255
     :fg 0
     :pt 5}))

(defn update-state [state]
  (let [current-state @exo/search-state]
    (cond (= (peek (:history state)) current-state) state
          (= current-state {}) (setup)
          :else (-> (assoc state :pos (count (:history state)))
                    (update :history conj current-state)))))

(defn node-x [i n w]
  (if (= n 1)
    0
    (q/map-range i 0 (dec n) (- w) w)))

(defn draw-event [ev x y pt]
  (let [[_ p o v] (re-matches rw-regex ev)
        texstr (tex/$ '(:langle) :iota \_ p \, (upper-case o) \( v \) :rangle)]
    (q/with-translation [x y]
      (q/scale 1)
      (q/push-style)
      (let [shp (latex texstr)]
        (q/no-stroke)
        (q/fill 255)
        (q/rect 0 0 (+ (.-width shp) pt) (+ (.-height shp) pt))
        (q/fill 0)
        (q/shape shp))
      (q/pop-style))))

(defn draw-search-state [state path w h pt]
  (let [{:keys [enabled] :as node} (state path)
        n (count enabled)
        w (/ w 2)
        nw (/ w (/ n 2.0))]
    (when-not (zero? n)
      (q/ellipse 0 0 10 10))
    (doseq [[i ev] (map-indexed vector enabled)]
      (let [x (node-x i n w)
            d (q/dist 0 0 x h)
            a (q/atan2 h x)
            x0 (* 2 pt (q/cos a))
            y0 (* 2 pt (q/sin a))
            x1 (* (/ d 2) (q/cos a))
            y1 (* (/ d 2) (q/sin a))
            x2 (* (- d (* 2 pt)) (q/cos a))
            y2 (* (- d (* 2 pt)) (q/sin a))
            next-path (conj path ev)]

        (q/push-style)
        (q/stroke 0)
        (when ((:sleep node) ev)
          (q/stroke 255 0 0))
        (when ((difference (:backset node) (:sleep node)) ev)
          (q/stroke 0 255  0))
        (q/line x0 y0 x1 y1)
        (when-not (empty? (mapcat identity (vals (state next-path))))          
          (q/line x1 y1 x2 y2))
        (q/pop-style)

        (draw-event ev x1 y1 pt)

        (q/translate x h)
        (draw-search-state state next-path nw h pt)
        (q/translate (- x) (- h))))))

(defn draw [{:keys [history pos bg fg pt]}]
  (q/background bg)
  (q/fill fg)
  (let [search-state (history pos)
        pad (* 0.05 (max (q/width) (q/height)))
        lengths (map count (keys search-state))
        depth (apply max 1 lengths)
        h (/ (- (q/height) (* 2 pad)) depth)
        w (/ (- (q/width) (* 2 pad)) 1.5)]
    (q/translate (/ (q/width) 2) pad)
    (draw-search-state search-state [] w h pt)))

(defn render-pdf [state]
  (let [w (* (q/display-density) (q/width))
        h (* (q/display-density) (q/height))
        path "resources/screenshots/out-%02d.pdf"]
    (clojure.java.io/make-parents path)
    (dotimes [pos (count (:history state))]
      (q/do-record
       (q/create-graphics w h :pdf (format path pos))
       (draw (setup (assoc state :pos pos))))))
  state)

(defn key-handler [state event]
  (case (q/key-as-keyword)
    :r (setup)
    :right (update state :pos #(mod (inc %) (count (:history state))))
    :left (update state :pos #(mod (dec %) (count (:history state))))
    :w (-> (assoc state :bg 255) (assoc :fg 0))
    :b (-> (assoc state :bg 0) (assoc :fg 255))
    :s (render-pdf state)
    state))

(defn run-sketch []
  (q/defsketch exo-viz
    :title "exo-viz"
    :host "host"
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :key-pressed key-handler
    :settings (fn [] (q/pixel-density (q/display-density)))
    :renderer :p2d
    :features [:no-bind-output :resizable]
    :middleware [m/fun-mode]))

#_(run-sketch)
