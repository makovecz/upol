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


(defun namaluj-semafor (sem)
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


#|
CL-USER 1 > (setf s (make-instance 'semafor))
#<SEMAFOR 200B6EDB>

CL-USER 2 > 
(namaluj-semafor s)
(T NIL)

CL-USER 3 > (nastav-dalsi-fazi s)
1

CL-USER 4 > (nastav-dalsi-fazi s)
0

CL-USER 5 > (nastav-dalsi-fazi s)
1

CL-USER 6 > (nastav-dalsi-fazi s)
0

CL-USER 7 > (namaluj-semafor s)
(T NIL)

CL-USER 8 > (nastav-dalsi-fazi s)
1

CL-USER 9 > (namaluj-semafor s)
(NIL T)

CL-USER 10 > (nastav-dalsi-fazi s)
0

CL-USER 11 > (namaluj-semafor s)
(T NIL)

CL-USER 12 > (setf (slot-value s 'seznam-svetel) svetla-3)
(:RED :ORANGE :GREEN)

CL-USER 13 > s
#<SEMAFOR 200B6EDB>

CL-USER 14 > (setf (slot-value s 'seznam-fazi) faze-3)
((T NIL NIL) (T T NIL) (NIL NIL T) (NIL T NIL))

CL-USER 15 > 
s
#<SEMAFOR 200B6EDB>

CL-USER 16 > (namaluj-semafor s)
(T NIL NIL)

CL-USER 17 > (nastav-dalsi-fazi s)
1

CL-USER 18 > (namaluj-semafor s)
(T T NIL)

CL-USER 19 > (nastav-dalsi-fazi s)
2

CL-USER 20 > (namaluj-semafor s)
(NIL NIL T)

CL-USER 21 > (nastav-dalsi-fazi s)
3

CL-USER 22 > (namaluj-semafor s)
(NIL T NIL)

CL-USER 23 > (nastav-dalsi-fazi s)
0

CL-USER 24 > (namaluj-semafor s)
(T NIL NIL)
|#

