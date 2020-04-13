(ns exogenous.visualize
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [render-latex.core :as tex]))

(def latex
  (memoize
   (fn [texstr]
     (let [svgfile (tex/latex->svg texstr)
           shp (q/load-shape svgfile)]
       (.disableStyle shp)
       shp))))

(defn setup []
  (q/rect-mode :center)
  (q/text-size 20)
  (q/text-align :center)
  (q/shape-mode :center)
  (q/stroke 0)
  {:instances '[1 2 n]})

(defn update-state [state]
  state)

(defn draw-boxed-shape
  ([shp] (draw-boxed-shape shp 0 0))
  ([shp x y] (draw-boxed-shape shp x y (.-width shp) (.-height shp)))
  ([shp x y w h]
   (q/push-style)
   (q/no-fill)
   (q/rect x y (+ w 20) (+ h 20) 10 10 10 10)
   (q/no-stroke)
   (q/fill 0)
   (q/shape shp x y w h)
   (q/pop-style)))

#_(defn draw [{:keys [history pos bg fg pt]}]
    (q/background 255)
    (q/with-translation [(q/width) (/ (q/height) 2)]
      (q/ellipse 0 0 100 100)
      (let [r (* 9/10 (q/width))]
        (doseq [[l theta] [[1 (/ q/PI 15)] [2 (/ q/PI 30)] ['n (- (/ q/PI 15))]]]
          (q/with-rotation [theta]
            (q/translate (- r) 0)
            (draw-boxed-shape (latex (tex/$ 'PL_ l)))
            (q/translate (/ r 4) 0)
            (draw-boxed-shape (latex (tex/$ 'TL_ l)))))
        (doseq [theta [0 (/ q/PI -60) (/ q/PI -30)]]
          (q/with-rotation [theta]
            (q/ellipse (- r) 0 2 2))))))

(defn draw [{:keys [instances]}]
  (q/background 255)
  (let [n (count instances)
        plx (* 2/21 (q/width))
        tlx (* 5/21 (q/width))
        cox (* 12/21 (q/width))
        dpx (* 19/21 (q/width))
        ym (/ (q/height) 2)]
    (dotimes [i (count instances)]
      (let [y (/ (* (inc i) (q/height)) (inc n))]
        (q/line (+ plx 30) y (- tlx 30) y)
        (q/line (+ tlx 30) y
                (q/lerp tlx cox 0.8)
                (q/lerp y ym 0.8))
        (draw-boxed-shape (latex (tex/$ 'PL_ (instances i))) plx y)
        (draw-boxed-shape (latex (tex/$ 'TL_ (instances i))) tlx y)))
    (q/with-translation [plx (/ (* (+ (dec n) 1/2) (q/height)) (inc n))]
      (q/rotate (/ q/PI 2))
      (q/push-style)
      (q/no-stroke)
      (q/fill 0)
      (q/shape (latex (tex/$ :cdots)))
      (q/translate 0 (- plx tlx))
      (q/shape (latex (tex/$ :cdots)))
      (q/pop-style))
    (draw-boxed-shape (latex (tex/tex 'Coordinator)) cox ym)
    (draw-boxed-shape (latex (tex/tex 'DPOR)) dpx ym)
    (q/line (q/lerp cox dpx 0.2) ym (q/lerp cox dpx 0.85) ym)

    #_(q/with-translation [pad 0]
        (doseq [instance instances]
          (q/translate 0 pad)
          (draw-boxed-shape (latex (tex/$ 'PL_ instance)))))
    #_(q/with-translation [(* 2.5 pad) 0]
        (doseq [instance instances]
          (q/translate 0 pad)
          (draw-boxed-shape (latex (tex/$ 'TL_ instance)))))

    #_(dotimes [i 3]
        #_(q/ellipse (- r) 0 2 2))))

(defn render-pdf [state]
  (let [w (* (q/display-density) (q/width))
        h (* (q/display-density) (q/height))
        path "resources/screenshots/architecture.svg"]
    (clojure.java.io/make-parents path)
    (q/do-record
     (q/create-graphics w h :svg path)
     (draw (setup))))
  state)

(defn key-handler [state event]
  (case (q/key-as-keyword)
    :r (setup)
    :s (render-pdf state)))

(defn run-sketch []
  (q/defsketch architecture
    :title "architecture"
    :host "host"
    :size [(* 25 30) (* 9 30)]
    :setup setup
    :update update-state
    :draw draw
    :key-pressed key-handler
    :settings (partial q/pixel-density 1)
    :renderer :p2d
    :features [:no-bind-output :resizable :keep-on-top]
    :middleware [m/fun-mode]))

#_(run-sketch)
