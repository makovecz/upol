(defvar *gamedef*
 (list
  (list "Vitejte ve hre"  5 (list 1 "Jit do mistnosti 1" 2 "Jit do mistnosti 2"))
  (list "Mistnost 1"     -1 (list 3 "Jit do mistnosti 3" 4 "Jit do mistnosti 4"))
  (list "Mistnost 2"      0 (list 3 "Jit do mistnosti 3" 5 "Jit do mistnosti 5"))
  (list "Mistnost 3"     -1 (list 1 "Jit do mistnosti 1" 5 "Jit do mistnosti 5"))
  (list "Mistnost 4"      0 (list 1 "Jit do mistnosti 1" 5 "Jit do mistnosti 5"))
  (list "The end"         0 nil)
 )
)

(defun death ()
	(format t "~% Zemreli jste. Hra pro vas timto konci!")
)

(defvar *lives* 0)

(defun start ()
  (let ((position (nth 0 *gamedef*)))
    (loop
     (write-line (nth 0 position))
     (setq *lives* (+ *lives* (nth 1 position)))
     (write-string "Zivotu: ") (write *lives*) (write-char #\newline)
     (when (<= *lives* 0) (death) (return))
     (let ((directions (nth 2 position)) (i 0))
       (when (null directions) (return))
       (loop
         (when (<= (list-length directions) (* 2 i)) (return))
         (let ((direction_text (nth (+ (* 2 i) 1) directions)))
           (write (+ i 1)) (write-char #\:) (write-char #\Space) (write-line direction_text)
         )
         (incf i)
       )
     )
     (volba)
    )
  )
)

