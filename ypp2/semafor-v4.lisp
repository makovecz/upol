; potrebne knihovny a pomocne soubory
(load (current-pathname "./micro-graphics/load.lisp"))
(load (current-pathname "./zdrojove-soubory/04.lisp"))
(load (current-pathname "./zdrojove-soubory/04_light.lisp"))

;; class semafor
;; Atributy:
;; - typ semaforu
;;   - seznam svetel 
;;   - seznam fazi
;; - faze
;; 
;; Metody:
;; - dalsi-faze


;; funkce
;; - namaluj-semafor(window, semafor)
;; - 

;; Moznosti reprezentace typu semaforu
;;[A]
;;((:red nil nil) (:red :orange nil) (nil nil :green) (nil :orange nil))
;;[B]  <--- tuhle moznost berem!
;;(:red :orange :green)
;;((t nil nil) (t t nil) (nil nil t) (nil t nil))

(defvar svetla
	(list
		'()
		'(:red)
		'(:red :green)
		'(:red :orange :green)
	)
)

(defvar faze
	(list
		'()
		'(t)
		'((t nil) (nil t))
		'((t nil nil) (t t nil) (nil nil t) (nil t nil))
	)
)

(defclass semafor ()
	(
		(seznam-svetel :initform svetla-2)
		(seznam-fazi   :initform faze-2)
		(aktualni-faze :initform 0)
	)
)

(defmethod nastav-dalsi-fazi
	(
		(sem semafor)
	)
	(setf
		(slot-value sem 'aktualni-faze)
		(mod 
			(+ 1 (slot-value sem 'aktualni-faze))
			(length (slot-value sem 'seznam-fazi))
		)
	)
)

(defun vypis-stav-semaforu (sem)
	(let
		(
			(seznamf  (slot-value sem 'seznam-fazi)) 
			(aktualni (slot-value sem 'aktualni-faze))
		)
		(
			nth aktualni seznamf
		)
	)
)

(defvar radius 20)
(defun stred-svetla-x () (+ radius 5))
(defun sirka-boxu () (* 2 (stred-svetla-x)))

(defun stred-svetla-y (poradi) (+ (* poradi (sirka-boxu)) (stred-svetla-x)))
(defun vyska-boxu (pocet-svetel) (* pocet-svetel (sirka-boxu)))

(defun vytvor-rohy-krabice (pocet-svetel)
	(list
		(move (make-instance 'point) 0            0)
		(move (make-instance 'point) (sirka-boxu) 0)
		(move (make-instance 'point) (sirka-boxu) (vyska-boxu pocet-svetel))
		(move (make-instance 'point) 0            (vyska-boxu pocet-svetel))
	)
)

(defun vytvor-svetlo (barva poradi)
	(move
		(set-radius
			(set-on-color
				(make-instance 'light)
				barva
			)
			radius
		)
		(stred-svetla-x)
		(stred-svetla-y poradi)
	)
)

(defun vytvor-seznam-svetel (pocet-svetel)
	(let
		( 
			(barvy (nth pocet-svetel svetla))
			result 
		)
		(dotimes (i pocet-svetel)
			(setf result (cons (vytvor-svetlo (nth i barvy) i) result))
		)
		(reverse result)
	)
)

(defclass semafor_obrazek (picture)
	(
		(krabice :initform (make-instance 'polygon))
		(svetla  :initform (make-instance 'picture))
	)
)

(defmethod inicializuj-semafor ((sem semafor_obrazek) pocet-svetel)
	(set-items (slot-value sem 'krabice) (vytvor-rohy-krabice pocet-svetel) )
	(set-items (slot-value sem 'svetla)  (vytvor-seznam-svetel pocet-svetel) )
)

(defmethod zhasni-prvni-svetlo ((sem semafor_obrazek)) 
	(set-off
		(nth
			0
			(slot-value
				(slot-value sem 'svetla)
                'items
            )
		)
	)
)

(defmethod vykresli ((sem semafor_obrazek))
	(let
		((w (make-instance 'window)))
		(set-items sem (list (slot-value sem 'krabice) (slot-value sem 'svetla)))
		(set-shape w sem)
		(redraw w)
	)
)

; (setf s (make-instance 'semafor_obrazek))
; (inicializuj-semafor s)
; (zhasni-prvni-svetlo s)
; (vykresli s)