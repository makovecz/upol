; potrebne knihovny a pomocne soubory
(load (current-pathname "./micro-graphics/load.lisp"))
(load (current-pathname "./zdrojove-soubory/04.lisp"))
(load (current-pathname "./zdrojove-soubory/04_light.lisp"))


;prevede seznam souradnic na seznam instanci tridy point
(defun coords-to-points (coords)
  (when coords 
    (let ((coord (car coords)))
      (cons (move (make-instance 'point) 
                  (first coord)
                  (second coord))
            (coords-to-points (cdr coords))))))


; Semafor
(defclass semaphore (picture) 
  ((lights :initform (make-instance 'picture))
   (semaphore-phase :initform 0)))


(defun make-light (on-color off-color radius in)
  (let ((l (make-instance 'light)))
    (set-on-color l on-color)
    (set-off-color l off-color) 
    (set-onp l in)
    (set-filledp l t)
    (set-thickness l t)
    (set-radius l radius)))


;cteni slotu semaphore-phase
(defmethod light-count ((sem semaphore))
  (slot-value sem 'semaphore-phase))
      

;obdelnikovy podklad semaforu
(defun podklad (x y b)
  (let ((c (make-instance 'polygon)))
    (if (= b 3) (set-items c (coords-to-points '((5 145) (50 145) (50 5) (5 5))))
                (set-items c (coords-to-points '((5 100) (50 100) (50 5) (5 5)))))
    (set-color c :black)
    (set-filledp c t)
    (move c x y)
    c))


(defmethod set-light-count ((s semaphore) count)
  (unless (or (= count 2) (= count 3)) (error "Invalid light count"))
  (set-items s (append 
                (list (move (make-light :red :orange 20 nil ) 0 50))
                (if (= count 3) (list (move (make-light :orange :grey 20 nil ) 0 95)) nil)
                (list (move (make-light :green :grey 20 nil ) 0 140))))
  (move s 58 10)
(next-phase s)
  s)


(defmethod next-phase ((s semaphore))
  (let ((p (make-instance 'light)))
  (mapcar (lambda (a) (set-on a)) (items s))))


;Semafor
(defun semafor ()
  (let ((k (make-instance 'picture))
        (s (podklad 30 30 3))
        (p (set-light-count (make-instance 'semaphore) 3)))
    (set-items k (list p s))
    (move k 0 0)))


#|
(setf w (make-instance 'window))
(setf sem (make-instance 'semaphore))
(set-shape w (semafor))
(redraw w)
(next-phase sem)
(set-light-count sem 3)
(set-light-count sem 2)
(redraw w)
|#

