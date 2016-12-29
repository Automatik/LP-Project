;;;; -*- Mode: Lisp -*-

;; aggiungere controllo che mtd corrisponda esattamente al grado delle variabili
;; monomial-total-degree e monomial-vars-and-powers sono per forza da fare?
(defun is-monomial (m)
  (and (listp m) ;controllo che sia scritto nella forma corretta (m coeff td vars-powers)
       (eq 'm (first m)) ;controllo che ci sia la lettera M
       (numberp (second m))
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
              (alpha-char-p (char (string var) 0)))))) ;symbolp non va bene. Trovare altro modo più snello

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
  (let ((vps (varpowers m)))
    (if (null (every #'is-varpower vps))
        nil
      (sum-varpowers vps))))

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

;funziona solo con coefficiente come primo elemento
(defun as-monomial (m)
  (if (or (null (listp m))
          (eq 'expt (first m)))
      (add-m (as-mony m))
    (let ((coeff (car (cdr m)))
          (vars (mapcar 'as-mony (cdr (cdr m)))))
      (if (and (not (null coeff)) ;se coeff non nullo e lista inizia con operatore *
               (eq '* (first m)))
          (if (null (numberp coeff)) ;se il coeff è una variabile
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
  (cond ((numberp m) m) ;`('m 0 nil)
        ((symbolp m) `(v 1 ,m))
        ((and (listp m)
              (eq 'expt (first m))) (let ((var (second m))
                                          (exp (third m)))
                                      (if (or (null (numberp exp))
                                              (null (symbolp var)))
                                          nil
                                        `(v ,exp ,var))))))

(defun add-m (m)
  (cond ((numberp m) (list 'm  m 0 nil)) ;se m è un numero
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
    
#|(defun polyplus (p1 p2)
  (if (and (is-polynomial p1)
           (is-polynomial p2))
      (sum-lists (dividi (append (second p1) 
                                 (second p2))))))|#

#|(defun dividi (ms l)
  (if (listp ms)
      (if (not (null ms))
          (let ((m (first ms))
                (subl (presente m l)))
            (if (not (null subl))
                (append subl 
                        (list m))|#
      
  
           
      



