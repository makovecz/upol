; 1. Hra musi mit alespon 10 situaci.
; 2. Data o hre budou ulozena v globalni promenne. Samotny program nesmi byt na konkretni hre zavisly.
; 3. Je povoleno pouzit jeste dalsi globalni promennou na ulozeni situace, ve ktere se hrac zrovna nachazi.
; 4. Hra se ovlada funkcemi start a choose zpusobem uvedenym v ukazce.

(defvar *gamedef*
 (list
  (list "Jste na prahu temne mistnosti. Co udelate?" 5 (list 1 "Zasmatram rukou po zdi a zkusim nahmatat vypinac." 2 "Udelam krok do tmy."))
  (list "Po zdi lezla velka tarantule a kousla vas! Pripravilo vas to o zivot." -1 (list 1 "Znovu zasmatram rukou po zdi." 2 "Udelam krok do tmy."))
  (list "Nachazite se v mistnosti." 0 (list 3 "Prozkoumam mistnost." 4 "Zapalim louc."))
  (list "V mistnosti je tma, ze by se dala krajet. Co udelate?" 0 (list 4 "Zapalim louc." 5 "Pujdu rovne."))
  (list "Zapalili jste louc. Po stene leze velka tarantule. Co udelate?" 0 (list 6 "Smrti se nebojim, pavouci mi nevadi. Prozkoumam mistnost!" 7 "Zabit tarantuli."))
  (list "Ve tme jste slapnuli na rezavy hrebik. Prisli jste o zivot." -1 (list 5 "Pujdu rovne." 4 "Zapalim louc."))
  (list "Pri prozkoumavani mistnosti na vas tarantule zautocila. Prisli jste o zivot." -1 (list 7 "Zkusit zabit tarantuli." 6 "Znovu prozkoumat mistnost."))
  (list "Pristoupili jste ke stene a zapalili tarantuli. Tarantule je mrtva." 0 (list 9 "Najit vychod." 8 "Sebrat mrtvou tarantuli."))
  (list "Popel je stale horky, popalili jste si ruku. Prisli jste o zivot." -1 (list 8 "Dobreho nic nepali. Zkusim to znovu!" 9 "Najit vychod."))
  (list "Z mistnosti vedou dvoje dvere, ale ktere vybrat?" 0 (list 10 "Jit do dveri nalevo." 10 "Zvolim dvere vpravo."))
  (list "Uspesne jste dosli do cile. KONEC HRY" 0 nil)
 )
)

(defun death ()
    (format t "~%Zemreli jste. Hra pro vas timto konci!")
)

(defvar *lives* 0)

(defun choose (n)
    (loop
      (when (and (typep n 'integer) (> n 0) (> 3 n)) (return))
      (write-line "Nespravna volba.")
      (setq n (read))
    )
    n
)

(defun start ()
  (let ((position (nth 0 *gamedef*)))
    (loop
     (write-char #\newline)
     (write-line (nth 0 position))
     (setf *lives* (+ *lives* (nth 1 position)))
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
     (setf position (nth (nth (* 2 (- (choose (read)) 1)) (nth 2 position)) *gamedef*))
  )
)
)

