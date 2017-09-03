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


(defvar svetla-2 '(:red :green))
(defvar svetla-3 '(:red :orange :green))

(defvar faze-2 '((t nil) (nil t)))
(defvar faze-3 '((t nil nil) (t t nil) (nil nil t) (nil t nil)))

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


(defclass semafor_obrazek (picture)
	(
		(krabice :initform (make-instance 'polygon))
		(svetla  :initform (make-instance 'picture))
	)
)

(defun vytvor-rohy-krabice ()
	(list
		(move (make-instance 'point) 0  0)
		(move (make-instance 'point) 50 0)
		(move (make-instance 'point) 50 100)
		(move (make-instance 'point) 0  100)
	)
)

(defun vytvor-seznam-svetel ()
	(list
		(move
			(set-radius
				(make-instance 'light)
				20
			)
			25
			25
		)
		(move
			(set-radius
				(make-instance 'light)
				20
			)
			25
			75
		)
	)
)

(defmethod namaluj ((sem semafor_obrazek) pocet-svetel win)
	(set-items (slot-value sem 'krabice) (vytvor-rohy-krabice) )
	(set-items (slot-value sem 'svetla)  (vytvor-seznam-svetel) )
	(set-items sem (list (slot-value sem 'krabice) (slot-value sem 'svetla)))
	(set-shape win sem)

	(redraw win)
)