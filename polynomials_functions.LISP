;;;; -*- Mode: Lisp -*-

;; aggiungere controllo che mtd corrisponda esattamente al grado delle variabili
;; monomial-total-degree e monomial-vars-and-powers sono per forza da fare?
(defun is-monomial (m)
  (and (listp m) ;controllo che sia scritto nella forma corretta (m coeff td vars-powers)
       (eq 'm (first m)) ;controllo che ci sia la lettera M
       (realp (second m))
       (let ((mtd (third m))
             (vps (fourth m)))
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

;; varpower-power e varpower-symbol sono per forza da fare?
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((power (second vp))
             (var (third vp)))
         (and (integerp power)
              (>= power 0)
              (alpha-char-p (coerce var 'character)))))) ;symbolp non va bene.

;; poly-monomials è per forza da fare?
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (second p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

; da rivedere: (v 1 x)
(defun varpowers (m)
  (cond ((is-monomial m) (fourth m))))

(defun vars-of (m)
  (let ((vps (varpowers m)))
    (if (null (every #'is-varpower vps))
        nil
      (var-symbols vps))))

(defun var-symbols (vps)
  (if (null vps)
      nil
    (flatten (list (third (first vps)) (var-symbols (rest vps))))))

; manca in lispworks
(defun flatten (l)
  (if l
      (if (atom l) 
          (list l) 
        (mapcan #'flatten l))))


#|(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (T (append (flatten (first x))
                   (flatten (rest x)))))) |#


;; do per scontato che sia nella forma corretta di monomio?
(defun monomial-degree (m)
  (cond ((is-monomial m) (third m))))
  #|(let ((vps (varpowers m)))
    (if (null (every #'is-varpower vps))
        nil
      (sum-varpowers vps))))|#

(defun sum-varpowers (vps)
  (if (null vps)
      0
    (+ (second (first vps)) (sum-varpowers (rest vps)))))

(defun monomial-coefficient (m)
  (cond ((is-monomial m) (second m))))

(defun coefficients (p)
  (if (null (is-polynomial p))
      nil
    (let ((ms (second p)))
      (mapcar 'monomial-coefficient ms))))

; Manca ordinamento
(defun variables (p)
  (if (null (is-polynomial p))
      nil
    (let ((ms (second p)))
      (flatten (mapcar 'vars-of ms)))))   ;oppure: (apply 'vars-of 'ms)

(defun maxdegree (p)
  (if (null (is-polynomial p))
      nil
    (let ((ms (second p)))
      (apply 'max (mapcar 'monomial-degree ms)))))

(defun mindegree (p)
  (if (null (is-polynomial p))
      nil
    (let ((ms (second p)))
      (apply 'min (mapcar 'monomial-degree ms)))))

;funziona solo con coefficiente come primo elemento e non messo in un altra posizione
;VEDERE CASI TIPO COS(30)
(defun as-monomial (m)
  (if (or (null (listp m))
          (eq 'expt (first m)))
      (add-m (as-mony m))
    (let ((coeff (car (cdr m)))
          (vars (mapcar 'as-mony (cdr (cdr m)))))
      (if (and (not (null coeff)) ;se coeff non nullo e lista inizia con operatore *
               (eq '* (first m)))
          (if (null (or (and (listp coeff) ;se il coeff è una variabile
                              (eval coeff))
                         (realp coeff))) 
              (if (null (cdr (cdr m))) ;se non c'è qualcosa dopo il coeff(var) nella lista
                  (list 'm 1 (sum-varpowers (list (as-mony coeff)) (as-mony coeff)))
                (if (null (member nil vars)) ;se vars non contiene nessuna variabile scritta male(cioè valore NIL)
                    (list 'm
                          1
                          (sum-varpowers (append (list (as-mony coeff)) vars))
                          (append (list (as-mony coeff)) vars))
                  nil))
            (if (null (cdr (cdr m))) ;se non c'è qualcosa dopo il coeff(num) nella lista
                (list 'm coeff 0 nil)
              (if (null (member nil vars)) ;se vars non contiene nessuna variabile scritta male(cioè valore NIL)
                  (list 'm
                        coeff
                        (sum-varpowers vars)
                        vars)
                nil)))
        nil))))
          
(defun as-mony (m)
  (cond ((realp m) m) ;`('m 0 nil)
        ((and (null (listp m))
              (alpha-char-p (coerce m 'character))) `(v 1 ,m))
        ((and (listp m)
              (eq 'expt (first m))) (let ((var (second m))
                                          (exp (third m)))
                                      (if (or (null (integerp exp))
                                              (null (alpha-char-p (coerce var 'character))))
                                          nil
                                        `(v ,exp ,var))))))

(defun add-m (m)
  (cond ((realp m) (list 'm  m 0 nil)) ;se m è un numero
        ((and (listp m)              ;se m è una variabile
              (eq 'v (first m)))
         (list 'm 1 (second m) (list m)))))

(defun as-polynomial (p)
  (cond ((null (listp p)) (list 'poly (list (as-monomial p))))
        ((and (listp p)
              (or (eq 'expt (first p))
                  (eq '* (first p))))
         (list 'poly (list (as-monomial p))))
        ((or (eq '+ (first p))
             (eq '- (first p)))
         (let ((ms (cdr p)))
           (list 'poly
                 (mapcar 'as-monomial ms))))))
    
(defun polyplus (p1 p2)
  (if (and (is-polynomial p1)
           (is-polynomial p2))
      (list 'poly 
            (sum-lists (dividi (append (second p1) 
                                       (second p2))
                               '())))
    nil))

(defun polyminus (p1 p2)
  (if (and (is-polynomial p1)
           (is-polynomial p2))
      (list 'poly
            (sum-lists (dividi (append (second p1)
                                       (negate-monomials (second p2)))
                               '())))
    nil))

(defun polytimes (p1 p2)
  (if (and (is-polynomial p1)
           (is-polynomial p2))
      (list 'poly
            (sum-lists (dividi (monomials (list 'poly
                                                (multiplicate-polynomials (second p1)
                                                                          (second p2))))
                               '())))
    nil))

(defun multiplicate-polynomials (ms1 ms2)
  (if (and (listp ms1)
           (listp ms2))
      (if (not (null ms2))
          (append (multiplicate-monomials ms1 (first ms2)) 
                  (multiplicate-polynomials ms1 (rest ms2)))
        nil)
    nil))

(defun multiplicate-monomials (ms m)
  (if (listp ms)
      (if (not (null ms))
          (append (list (list 'm
                              (* (monomial-coefficient (first ms)) 
                                 (monomial-coefficient m))
                              (+ (monomial-degree (first ms))
                                 (monomial-degree m))
                              (sum-variables (varpowers (first ms)) (varpowers m))))
                  (multiplicate-monomials (rest ms) m))
        nil)
    nil))

(defun sum-variables (vs1 vs2)
  (if (and (listp vs1)
           (listp vs2))
      (cond ((and (null vs1)
                  (null vs2))
             nil)
            ((and (null vs1)
                  (not (null vs2)))
             vs2)
            ((and (not (null vs1))
                  (null vs2))
             vs1)
            ((and (not (null vs1))
                 (not (null vs2)))
            (let ((vp1 (extract-variable (first vs1) vs1))
                  (vp2 (extract-variable (first vs1) vs2)))
              (append (list (list 'v
                                  (sum-varpowers (append vp1 vp2))
                                  (third (first vp1))))
                      (sum-variables (set-difference vs1 vp1 :test 'equal) 
                                     (set-difference vs2 vp2 :test 'equal))))))
    nil))

(defun extract-variable (v vs)
  (if (listp vs)
      (cond ((null vs) nil)
            ((and (not (null vs))
                  (eq (third v) (third (first vs))))
             (append (list (first vs)) (extract-variable v (rest vs))))
            (T (extract-variable v (rest vs))))
    nil))

(defun sum-lists (ms)
  (if (and (listp ms)
           (not (null ms)))
      (append (list (sum-monomials (first ms))) (sum-lists (rest ms)))
    nil))

(defun sum-monomials (ms)
  (if (listp ms)
      (if (not (null ms))
          (let ((m (first ms)))
            (list 'm
                  (+ (monomial-coefficient m)
                     (monomial-coefficient (sum-monomials (rest ms))))
                  (monomial-degree m)
                  (varpowers m)))
        (list 'm 0 0 nil))
    nil))

(defun dividi (ms l)
  (if (listp ms)
      (if (not (null ms))
          (let ((m (first ms))
                (subl (presente (first ms) l)))
            (if (not (null subl)) ;base letterale di m già presente nella lista
                (dividi (rest ms)
                        (append (remove-sublist l subl)
                                (list (append subl (list m)))))
              (dividi (rest ms) ;aggiungo m con la sua base letterale alla lista
                      (append l (list (list m))))))
        l)
    nil))
              

(defun remove-sublist (l sublist)
  (if (and (listp l)
           (listp sublist)
           (not (null l)))
      (if (equal sublist (first l))
          (remove-sublist (rest l) sublist)
        (append (list (first l)) (remove-sublist (rest l) sublist)))
    nil))

(defun presente (m l)
  (if (and (listp l)
           (not (null l)))
           ;(is-monomial m))
      (let ((sublist (first l))
            (monomial (first (first l))))
        (if (and (eq (monomial-degree m) (monomial-degree monomial))
                 (equal (varpowers m) (varpowers monomial)))
            sublist
          (presente m (rest l))))
    nil))

(defun negate-monomials (ms)
  (if (and (listp ms)
           (not (null ms)))
      (let ((m (first ms))
            (coeff (monomial-coefficient (first ms))))
        (append (list (list 'm
                            (- coeff)
                            (monomial-degree m)
                            (varpowers m)))
                (negate-monomials (rest ms))))
    nil))

;VariableValues è una lista anche nel caso di un solo valore? es ((v 1 x)) VV=3 o (3)?
(defun polyval (p varvalues)
  (if (and (is-polynomial p)
           (every #'realp varvalues))
      (let ((vars (variables p)))
        (calcola (sostituisci (second p) vars varvalues)))
    nil))

(defun sostituisci (ms vars varvalues)
  (if (and (listp ms)
           (listp vars)
           (listp varvalues))
      (if (not (null ms))
          (let ((vs (varpowers (first ms))))
            (append (list (append (remove-sublist (first ms) vs)
                            (list (cambia vs vars varvalues))))
                    (sostituisci (rest ms) vars varvalues)))
        nil)
    nil))

(defun cambia (vs vars varvalues)
  (if (and (listp vs)
           (listp vars)
           (listp varvalues))
      (if (not (null vs))
          (append (list (list 'v 
                              (second (first vs))
                              (nth (position (third (first vs)) vars)
                                   varvalues)))
                  (cambia (rest vs) vars varvalues))
        nil)
    nil))

(defun calcola (ms)
  (if (listp ms)
      (if (not (null ms))
          (+ (* (second (first ms))
                (calcola-vars (fourth (first ms))))
             (calcola (rest ms)))
        0)
    0))

(defun calcola-vars (vs)
  (if (listp vs)
      (if (not (null vs))
          (* (expt (third (first vs)) (second (first vs)))
             (calcola-vars (rest vs)))
        1)
    1))    
  
(defun monomials (p)
  (if (null (is-polynomial p))
      nil
    (append (list 'poly) (list (ordina-stesso-grado (ordina-monomi (mergesort (second p))) (mindegree p)))))) 
  

(defun list-head (lst n) (if (eq n 0) '() (cons (car lst) (list-head (cdr lst) (- n 1)))))
(defun list-tail (lst n) (if (eq n 0) lst (list-tail (cdr lst) (- n 1))))

(defun _merge (lst-a lst-b)
  (cond ((not lst-a) lst-b)
        ((not lst-b) lst-a)
        ((< (monomial-degree (car lst-a)) (monomial-degree (car lst-b))) (cons (car lst-a) (_merge (cdr lst-a) lst-b)))
        (T (cons (car lst-b) (_merge lst-a (cdr lst-b))))))

(defun mergesort (lst)
  (if (eq (length lst) 1)
    lst
    (_merge (mergesort (list-head lst (truncate (length lst) 2)))
            (mergesort (list-tail lst (truncate (length lst) 2))))))
 
;(defun ordina-monomi (p) 
  #|(if (null (second p))
      nil
  (append (list 'poly) (append ((first p) (second p) (third p) (mergesort2 (fourth (car (second p)))))  (ordina-monomi(second p))))))|#
(defun ordina-monomi (ms)
  (if (null ms)
      nil
    (let ((m (first ms))
          (vs (varpowers (first ms))))
      (append (list (append (remove-sublist m vs)
                      (list (mergesort2 vs))))
              (ordina-monomi (rest ms))))))


(defun _merge2 (lst-a lst-b)
  (cond ((not  lst-a) lst-b)
        ((not  lst-b) lst-a)
        #|((null (car lst-a)) lst-b)
        ((null (car lst-b)) lst-a)
        ((and (null (car lst-a)) ;caso tutte due NIL, vince a caso lst-a
              (null (car lst-b))) lst-a) |#
        ;((null (car lst-a)) (cons (car lst-b) (_merge2 lst-a (cdr lst-b))))
        ;((null (car lst-b)) (cons (car lst-a) (_merge2 (cdr lst-a) lst-b)))
        ((char< (char (string (third (car lst-a))) 0) (char (string (third (car lst-b))) 0) ) (cons (car lst-a) (_merge2 (cdr lst-a) lst-b)))
        (T (cons (car lst-b) (_merge2 lst-a (cdr lst-b))))))

(defun mergesort2 (lst)
  #|(if (eq (length lst) 1)
    lst
    (_merge2 (mergesort2 (list-head lst (truncate (length lst) 2)))
            (mergesort2 (list-tail lst (truncate (length lst) 2))))))|#
  (cond ((null lst) nil)
        ((eq (length lst) 1) lst)
        (T (_merge2 (mergesort2 (list-head lst (truncate (length lst) 2)))
                    (mergesort2 (list-tail lst (truncate (length lst) 2)))))))

(defun pprint-polynomial (p) 
  (if (null (is-polynomial p))
      nil
    (format t "~s" (print-monomials (second p)))))

(defun print-monomials (ms) 
  (if (listp ms)
      (if (not (null ms))
          (let ((c (monomial-coefficient (first ms)))
                (vs (varpowers (first ms))))
            (cond ((> c 1) (concatenate 'string "+" (write-to-string c) " "
                                        (print-vars vs)
                                        (print-monomials (rest ms))))
                  ((< c 0) (concatenate 'string (write-to-string c) " "
                                        (print-vars vs)
                                        (print-monomials (rest ms))))
                  ((eq c 1) (concatenate 'string  " +"
                                        (print-vars vs)
                                        (print-monomials (rest ms))))))
            ;(format t " ~d" c)
            ;(print-vars vs)
            ;(print-monomials (rest ms)))
            ;(format t "~d ~s ~s" c (print-vars vs) (print-monomials (rest ms))))
        nil)
    nil))

(defun print-vars (vs) 
  (if (listp vs)
      (if (not (null vs))  
                       (let ((power (second (first vs)))
                             (var (third (first vs))))
                         (cond ((eq power 1) (concatenate 'string (string var) " "))
                               ;((> power 1) (format t " ~s ^ ~d" var power))
                               ;((< power 0) (format t " ~s ^(- ~d)" var power))))
                               ((> power 1) (concatenate 'string (string var)  "^" (write-to-string power) " " (print-vars (rest vs))))
                               ((< power 0) (concatenate 'string (string var)  "^ -(" (write-to-string power) ") "(print-vars (rest vs))))))
          nil)
    nil))

(defun ordina-stesso-grado (p g) 
  (if (<= g (maxdegree (append (list 'POLY p) )))
      (append (mergesort3 (estrai-stesso-grado p g)) (ordina-stesso-grado p (+ g 1))) 
    nil))

(defun estrai-stesso-grado (p g) 
  (cond ((null (is-monomial (car p)))
         nil)
        ((eq (third (car p)) g) 
         (append (list (car p)) (estrai-stesso-grado (rest p) g)))
        (T (estrai-stesso-grado (rest p) g))))
         
(defun _merge3 (lst-a lst-b)
  (cond ((not lst-a) lst-b)
        ((not lst-b) lst-a)
        ((and (eq (third (car lst-a)) 0) (< (second (car lst-a)) (second (car lst-b))))
         (cons (car lst-a) (_merge3 (cdr lst-a) lst-b)))
        ((and (eq (third (car lst-a)) 0) (> (second (car lst-a)) (second (car lst-b))))
         (cons (car lst-b) (_merge3 lst-a (cdr lst-b))))
        
        ((and (confronto-liste (fourth (car lst-a)) (fourth (car lst-b))) (< (second (car lst-a)) (second (car lst-b))))
         (cons (car lst-a) (_merge3 (cdr lst-a) lst-b)))
        ((and (confronto-liste (fourth (car lst-a)) (fourth (car lst-b))) (> (second (car lst-a)) (second (car lst-b))))
         (cons (car lst-b) (_merge3 lst-a (cdr lst-b))))

        ((paragona-var-powers (fourth (car lst-a)) (fourth (car lst-b)))  (cons (car lst-a) (_merge3 (cdr lst-a) lst-b)))
        (T (cons (car lst-b) (_merge3 lst-a (cdr lst-b))))))

(defun mergesort3 (lst)
  (cond ((null lst) nil)
        ((eq (length lst) 1) lst)
        (T (_merge3 (mergesort3 (list-head lst (truncate (length lst) 2)))
                    (mergesort3 (list-tail lst (truncate (length lst) 2)))))))
  
(defun paragona-var-powers (a b)
  (cond ((char< (char (string (third (car a))) 0) (char (string (third (car b))) 0) )
         T)
        ((char> (char (string (third (car a))) 0) (char (string (third (car b))) 0) )
         nil)
        ((null a)
         nil)
        ((null b)
         T)
        ((confronto-liste (var-symbols a) (var-symbols b))
         (paragona-powers a b))
        ((eq (third (car a)) (third (car b)))
         (paragona-var-powers (rest a) (rest b)))))

(defun paragona-powers (a b)
  (cond ((< (second (first a)) (second (first b)))
         T)
        ((> (second (first a)) (second (first b)))
         nil)
        ((and (eq (second (first a)) (second (first b))) (null (rest a)))
         T)
        ((eq (second (first a)) (second (first b)))
         (paragona-powers (rest a) (rest b)))))

(defun confronto-liste (a b)
  (if (eq (length a) (length b))
      (cond ((null (rest a))
             (if (eq (first a) (first b))
                 T
               nil))
            ((eq (first a) (first b))
             (confronto-liste (rest a) (rest b)))
            (T nil))
    nil))
             
         
         