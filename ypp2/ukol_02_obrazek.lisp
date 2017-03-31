; potrebne knihovny a pomocne soubory
(load (current-pathname "./micro-graphics/load.lisp"))
(load (current-pathname "./zdrojove-soubory/03.lisp"))

; pomocna pracovni funkce na vykresleni obrazku
(defun pic ()
  (setf w (make-instance 'window))
  (setf p (make-my-picture))
  (set-shape w p)
  (redraw w)
)

; prevod radianu na stupne
(defun rtd (r) (* 180.0 (/ r pi)))

; prevod stupnu na radiany
(defun dtr (d) (* pi (/ d 180.0)))

; definujte funkci make-my-picture (kapitola 3, zapouzdreni/polymorfismus)
; obrazek musi obsahovat alespon jednu instanci kazde ze trid:
; circle, picture, polygon

(defun make-my-picture ()
  (let
      ((my-circle (make-instance 'circle))
       (my-picture (make-instance 'picture)) 
       (my-polygon (make-instance 'polygon))
       (my-point (mapcar (lambda (x)
            (apply #'move (make-instance 'point) x))
            ; 09-examples.lisp - priklady pouziti graficke knihovny clmg 
            ; slo by i v letu, pomoci car/cdr
                         '((80 90) (40 100) (0 70) (40 50) (0 30) (40 0) (80 10) (100 50)))))
    ; musi pouzivat alespon ctyri ze zprav:
    ; set-color, set-thickness, set-filledp, move, rotate, scale
    (set-color (set-thickness (set-items my-polygon my-point) 5) :blue)
    (set-color (set-filledp (set-radius (move my-circle 110 110) 10) 't) :aquamarine) 
    (rotate (set-items my-polygon my-point) (dtr 180) (first (items my-polygon)))
    (set-items my-picture (list my-circle my-polygon))
  )
)
