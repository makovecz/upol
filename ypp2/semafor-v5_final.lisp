; potrebne knihovny a pomocne soubory
(load (current-pathname "./micro-graphics/load.lisp"))
(load (current-pathname "./zdrojove-soubory/04.lisp"))
(load (current-pathname "./zdrojove-soubory/04_light.lisp"))

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

(defun rozsvit-nebo-zhasni (svetlo on_off)
	(if on_off (set-on svetlo))
	(if (not on_off) (set-off svetlo))
)

(defclass semafor (picture)
	(
		(krabice    :initform (make-instance 'polygon))
		(svetla     :initform (make-instance 'picture))
		(cislo-faze :initform 0)
	)
)

(defmethod inicializuj-semafor ((sem semafor) pocet-svetel)
	(set-items (slot-value sem 'krabice) (vytvor-rohy-krabice pocet-svetel) )
	(set-items (slot-value sem 'svetla)  (vytvor-seznam-svetel pocet-svetel) )
	(rozsvit-svetla-podle-faze sem)
)

(defmethod rozsvit-svetla-podle-faze ((sem semafor))
	(let* ;; ta hvezdicka => v druhe casti 'let' se muze rovnou pouzit svetla-items
		(
			( svetla-items  (slot-value (slot-value sem 'svetla) 'items)  )
			( aktualni-faze (nth (slot-value sem 'cislo-faze) (nth (length svetla-items) faze)) )
		)
		(mapcar 'rozsvit-nebo-zhasni svetla-items aktualni-faze)
	)
)

(defmethod nastav-dalsi-fazi ( (sem semafor) )
	(let 
		(
			(pocet-fazi
				(length
					(nth
						(length
							(slot-value
								(slot-value sem 'svetla)
								'items
							)
						)
						faze
					)
				)
			)
		)
		
		(setf
			(slot-value sem 'cislo-faze)
			(mod 
				(+ 1 (slot-value sem 'cislo-faze))
				pocet-fazi
			)
		)
		(rozsvit-svetla-podle-faze sem)
        )
)


(defmethod vykresli ((sem semafor))
	(let
		((w (make-instance 'window)))
		(set-items sem (list (slot-value sem 'krabice) (slot-value sem 'svetla)))
		(set-shape w sem)
		(redraw w)
	)
)

; (setf s (make-instance 'semafor))
; (inicializuj-semafor s 2)
; (vykresli s)
; (nastav-dalsi-fazi s)
; (vykresli s)
; (nastav-dalsi-fazi s)
; (vykresli s)
