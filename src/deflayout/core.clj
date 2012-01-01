
(ns deflayout.core
  (:import (java.awt BorderLayout GridBagLayout FlowLayout)))

(defn layout-mapping
  "Returns a classname for given k."
  [k]
  (cond
   (= k :border) BorderLayout
   (= k :flow) FlowLayout))
  
(defn keyword-to-field
  "If mb-keyword is a keyword, returns the construction like (. classname mb-keyword), otherwise returns mb-keyword."
  [mb-keyword classname]
  (if (keyword? mb-keyword)
    `(. ~classname ~(symbol (name mb-keyword)))
    mb-keyword))

(defmulti expand-layout
  "Multimethod that expands macro body based on given layout"
  (fn[layout & _] layout))

(defmethod expand-layout BorderLayout [layout body]
           (map (fn[[param obj]]
                  `(.add ~obj
                         ~(keyword-to-field param layout)))
                body))

(defmethod expand-layout FlowLayout [layout body]
           (map (fn[obj]
                  `(.add ~obj)) body))

(defmacro deflayout
  "Transforms given set of UI declarations into executable code. Example of usage:

(let [frame (JFrame.)
      panel (JPanel.)]
  (deflayout frame (:border)
    {:CENTER (deflayout panel (:flow :LEADING)
               [(JButton.)
                (JButton.)])}))

In this example, we first create a frame and a panel. Then we call our macro on a frame and specify that we want BorderLayout on it. :border keyword is taken into parentheses so you can provide additional arguments to its constructor. Then you state that you want a panel to be added to your frame's center. Panel definition is itself a macro call that sets the panel to FlowLayout and puts two buttons onto it."
  [container layout-list body]
  (let [layout# (layout-mapping (first layout-list))]
    `(doto ~container
       (.setLayout (new ~layout# ~@(map (fn[k] (keyword-to-field k layout#)) (drop 1 layout-list))))
       ~@(expand-layout layout# body))))