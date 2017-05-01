; potrebne knihovny a pomocne soubory
(load (current-pathname "./micro-graphics/load.lisp"))
(load (current-pathname "./zdrojove-soubory/04.lisp"))

(defun jp-flag-items ()
  (let ((circle (make-instance 'circle))
        (poly1 (make-instance 'polygon))
        (poly2 (make-instance 'polygon)))
    (set-radius circle 36)
    (move circle 110 80)
    (set-filledp circle t)
    (set-color circle (color:make-rgb (/ #xce 255.0)    ;#xce je hexadecimálně 206
                                      (/ #x11 255.0)    ;17
                                      (/ #x26 255.0)))  ;38

    (set-items poly1 (mapcar (lambda (coords)
                               (apply 'move (make-instance 'point) coords))
                             '((20 20) (200 20) (200 140) (20 140))))
    (set-items poly2 (mapcar (lambda (coords)
                               (apply 'move (make-instance 'point) coords))
                             '((20 20) (200 20) (200 140) (20 140))))
    (set-color poly2 :white)
    (set-filledp poly2 t)

    (list circle poly1 poly2)))

(defun make-jp-flag ()
  (set-items (make-instance 'picture)
             (jp-flag-items)))

(defun display-jp-flag-window ()
  (redraw (set-shape (set-background (make-instance 'window) :skyblue)
                     (make-jp-flag))))

#|
;; Testy: (vyhodnocujte postupně každý řádek)

(setf w (display-jp-flag-window))
(move (shape w) 20 20)
(redraw w)
(setf center (center (first (items (shape w)))))
(scale (shape w) 1.2 center)
(redraw w)
(rotate (shape w) (/ pi 4) center)
(redraw w)
|#
