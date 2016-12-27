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

(defun varpowers (m)
  (cond ((is-monomial m) (fourth m))))

(defun vars-of (m)
  (let ((vps (varpowers m)))
    (if (null (every #'is-varpower vps))
        null
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

;; do per scontato che sia nella forma corretta di monomio?
(defun monomial-degree (m)
  (let ((vps (varpowers m)))
    (if (null (every #'is-varpower vps))
        null
      (sum-varpowers vps))))

(defun sum-varpowers (vps)
  (if (null vps)
      0
    (+ (second (first vps)) (sum-varpowers (rest vps)))))

(defun monomial-coefficient (m)
  (cond ((is-monomial m) (second m))))

(defun coefficients (p)
  (if (null (is-polynomial p))
      null
    (let ((ms (second p)))
      (mapcar 'monomial-coefficient ms))))

; Manca ordinamento
(defun variables (p)
  (if (null (is-polynomial p))
      null
    (let ((ms (second p)))
      (flatten (mapcar 'vars-of ms)) #'symbol<)))
      



