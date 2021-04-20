
;;--------------------------=={ Calculo Esforco }==------------------------;;
;;  Este programa calcula o esforco de um cabo de fibra optica (sem cordo  ;;
;;  alha ou com cordoalha de aco ou dieletrica) em um determinado poste,   ;;
;;  seguindo as normas da CPFL                                             ;;
;;-------------------------------------------------------------------------;;
;;     Author:  Paulo Hortelan Ribeiro, Copyright � 2020                  ;;
;;-------------------------------------------------------------------------;;
;;  Version 1.0    -    20/04/2021                                         ;;
;;                                                                         ;;
;;  - First release.                                                       ;;
;;-------------------------------------------------------------------------;;

(defun C:CalcEsforco(/ *error* dest i e ss ht suma enti oldzin pct prec val p1 suma2 a ht2 e2 somavaos 
        bb1 bb2 blk bnm bpt
        def dis
        ent
        fac
        gr1 gr2
        idx inc
        llp lst
        mat msg
        obj ocs oss
        pi2 pt1 pt2 pt3 pt4
        sel sel2
        tma tmp trm
        urp uxa
        vec
        ent-text ent-leader
        forc1 forc1-str forc2 forc2-str
        result tmp tmp2 tmp3 tmp4)

;;--------------------------=={ Object Align }==------------------------;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2010  -  www.lee-mac.com             ;;
;;----------------------------------------------------------------------;;

(defun oa ( oss p1 )

    (defun *error* ( msg )
        (if (and (= 'list (type trm)) (= 'ename (type ent)) (entget ent))
            (entdel ent)
        )
        (if (and (= 'vla-object (type blk)) (not (vlax-erased-p blk)))
            (vl-catch-all-apply 'vla-delete (list blk))
        )
        (if (and (= 'vla-object (type def)) (not (vlax-erased-p def)))
            (vl-catch-all-apply 'vla-delete (list def))
        )
        (foreach obj lst
            (if (not (vlax-erased-p obj))
                (vl-catch-all-apply 'vla-delete (list obj))
            )
        )
        (oa:endundo (oa:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (oa:startundo (oa:acdoc))
    (if (null oa|rot) (setq oa|rot 0.0))
    (if (null oa|off) (setq oa|off 0.0))
    
    (cond
        (   (or (oa:layerlocked (getvar 'clayer))
                (oa:layerlocked "0")
            )
            (princ "\nThe current layer or layer \"0\" is locked - please unlock these layers before using this program.")
        )
        (   (progn
                (setq bpt p1)
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\nSelecione a curva para alinhar <exit>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nNenhum objeto foi selecionado. Tente novamente.")
                            )
                            (   (= 'ename (type (car sel)))
                                (if
                                    (not
                                        (or (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getendparam (list (car sel)))))
                                        )
                                    )
                                    (princ "\nObjeto selecionado invalido.")
                                )
                            )
                        )
                    )
                )
                (while (/= 5 (car (setq pt1 (grread t 13 1)))))
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq trm (caddr sel))
                        (setq ent (oa:copynested (car sel) trm))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\nUnable to recreate nested entity.")
        )
        (   (progn
                (setq ocs (trans '(0 0 1) 1 0 t)
                      uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
                      mat (mxm
                              (list
                                  (list (cos uxa)     (sin uxa) 0.0)
                                  (list (- (sin uxa)) (cos uxa) 0.0)
                                 '(0.0 0.0 1.0)
                              )
                              (mapcar '(lambda ( a ) (trans a ocs 0 t))
                                 '(
                                      (1.0 0.0 0.0)
                                      (0.0 1.0 0.0)
                                      (0.0 0.0 1.0)
                                  )
                              )
                          )
                      vec (mapcar '- (mxv mat (trans '(0.0 0.0 0.0) ocs 0)))
                      tma (vlax-tmatrix (append (mapcar 'append mat (mapcar 'list vec)) '((0.0 0.0 0.0 1.0))))
                )
                (repeat (setq idx (length oss))                    
                    (setq idx (1- idx)
                          obj (vla-copy (vlax-ename->vla-object (cdr (assoc -1 (nth idx oss)))))
                          lst (cons obj lst)
                    )
                    (vla-transformby obj tma)
                    (if (and (vlax-method-applicable-p obj 'getboundingbox)
                             (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                        )
                        (setq bb1 (cons (vlax-safearray->list llp) bb1)
                              bb2 (cons (vlax-safearray->list urp) bb2)
                        )
                    )
                    (vla-delete (vlax-ename->vla-object (cdr (assoc -1 (nth idx oss)))))
                    (vla-put-visible obj :vlax-false)
                )
                (not (and bb1 bb2))
            )
            (*error* nil)
            (princ "\nUnable to calculate bounding box for the selection.")
        )
        (   t
            (setq bb1 (apply 'mapcar (cons 'min bb1))
                  bb2 (apply 'mapcar (cons 'max bb2))
                  bpt (cond ( bpt (mapcar '+ (mxv mat (trans bpt 1 0)) vec)) ((mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) bb1 bb2)))
                  fac (/ (- (cadr bb2) (cadr bb1)) 2.0)
                  pi2 (/ pi -2.0)
                  inc 0
            )
            (if (equal 0.0 fac 1e-8)
                (if (equal bb1 bb2 1e-8)
                    (setq fac 1.0)
                    (setq fac (/ (- (car bb2) (car bb1)) 2.0))
                )
            )
            (while (tblsearch "block" (setq bnm (strcat "$tmp" (itoa (setq inc (1+ inc)))))))
            (foreach obj lst (vla-put-visible obj :vlax-true))
            (vla-copyobjects (oa:acdoc)
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst))))
                        lst
                    )
                )
                (setq def (vla-add (vla-get-blocks (oa:acdoc)) (vlax-3D-point bpt) bnm))
            )
            (foreach obj lst (vla-delete obj))
            (setq lst nil
                  blk
                (vla-insertblock
                    (vlax-get-property (oa:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans (cadr pt1) 1 0))
                    bnm 1.0 1.0 1.0 0.0
                )
            )
            (vla-put-layer  blk "0")
            (vla-put-normal blk (vlax-3D-point ocs))
            (setq msg (princ "\n[+/-] para [O]ffset | [</>] para [R]otation | [M]ultiple | <[E]xit>: "))

            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (member gr1 '(3 5))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointtoprojection ent pt2 ocs)
                                  pt3 (oa:2d (trans pt1 0 ocs))
                                  pt4 (oa:2d (trans pt2 0 ocs))
                            )
                            (if (not (equal pt3 pt4 1e-8))
                                (progn
                                    (setq dis (/ (* fac oa|off) (distance pt3 pt4)))
                                    (vla-put-insertionpoint blk
                                        (vlax-3D-point
                                            (trans
                                                (append
                                                    (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt3 pt4)
                                                    (list (caddr (trans pt1 0 ocs)))
                                                )
                                                ocs 0
                                            )
                                        )
                                    )
                                    (vla-put-rotation blk (+ (angle (trans pt1 0 ocs) (trans gr2 1 ocs)) oa|rot pi2))
                                )
                            )
                            (cond
                                (   (= 5 gr1))
                                (   (progn (vla-explode blk) oa|mtp))
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq oa|off (+ oa|off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq oa|off (- oa|off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq oa|rot (+ oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq oa|rot (- oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    nil
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\nSpecify Rotation <" (angtos oa|rot) ">: ")))
                                        (setq oa|rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\nSpecify Offset <" (rtos (* fac oa|off)) ">: ")))
                                        (setq oa|off (/ tmp fac))
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(077 109))
                                    (if (setq oa|mtp (not oa|mtp))
                                        (princ "\n<Multiple mode on>")
                                        (princ "\n<Multiple mode off>")
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(011 025))
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if trm (entdel ent))
            (vla-delete  blk)
            (vla-delete  def)
            (oa:endundo (oa:acdoc))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun oa:2d ( x ) (list (car x) (cadr x)))

;;----------------------------------------------------------------------;;

(defun oa:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;----------------------------------------------------------------------;;

(defun oa:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (oa:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (oa:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (oa:entmakex enx)))))
        )
        (setq tmp (oa:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;----------------------------------------------------------------------;;

(defun oa:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
                (function
                    (lambda ( x )
                        (or (member (car x) '(005 006 008 039 048 062 102 370))
                            (= 'ename (type (cdr x)))
                        )
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun oa:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun oa:startundo ( doc )
    (oa:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun oa:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun oa:acdoc nil
    (eval (list 'defun 'oa:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (oa:acdoc)
)

;;----------------------------------------------------------------------;;

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
    
;;----------------------------------------------------------------------;;

(vl-load-com)

;;----------------------------------------------------------------------;;
;;                      End of function Object Align                    ;;
;;----------------------------------------------------------------------;;



;;----------------------------------------------------------------------;;
;;----------------------------------------------------------------------;;



;;----------------------------------------------------------------------;;
;;                      Inicio da funcao CalcEsforco                    ;;
;;----------------------------------------------------------------------;;

  ;;Definicao de constantes

    ;;Peso de cada fibra - Sterlite
    (setq fb12-peso 0.049) ;Cabo de 12 fibras ;;preciso ajustar
    (setq fb24-peso 0.069) ;Cabo de 24 fibras
    (setq fb36-peso 0.069) ;Cabo de 36 fibras

    ;;Diametro de cada fibra - Sterlite
    (setq fb12-dm 8) ; Cabo de 12 fibras ;; preciso ajustar
    (setq fb24-dm 9.8) ; Cabo de 24 fibras
    (setq fb36-dm 9.8) ; Cabo de 36 fibras

    ;;Velocidade do vento em Km/h
    (setq vel-vent 94)

    ;;Dados da cordoalha dieletrica(1/4 - FRP) - Fibracem
    (setq cord-dieletrica-peso 0.043)
    (setq cord-dieletrica-dm 6.35)

    ;;Dados da cordoalha de aco(3/16 - 7 fios) - Eletro Luminar
    (setq cord-aco-peso 0.108)
    (setq cord-aco-dm 4.76)

  ;; Funcao de erro
  (defun *error* (msg)
    (and msg (/= msg "Funcao cancelada") (princ msg))
  )

  ;; Funcao para arredondar um numero para uma casa decimal
  (defun myrtos ( x / dim rtn )
    (setq dim (getvar 'dimzin))
    (setvar 'dimzin 4)
    (setq rtn (vl-catch-all-apply 'rtos (list x 2 (if (< x 1) 2 1))))
    (setvar 'dimzin dim)
    (if (not (vl-catch-all-error-p rtn)) rtn)
  )

  ;; Funcao que substitui um character por outro
  (defun subs-string  (str old new / a b)
    (while (and (setq a (substr str 1 1))
                (> (strlen str) 0))
            (setq b   (strcat (if (null b) "" b)
                              (if (eq old a)
                                    new
                                    a))
                  str (substr str 2))
          )
    b
  )

  ;; Funcao para splitar string
  (defun split ( s d )
    ( (lambda (f) (if (/= "" d) (f s d "" nil)))
      (lambda ( s d c b / k )
        (if (/= s "")
          (if (= d (setq k (substr s 1 1))) 
            (append 
              (cond 
                ( (/= c "") (list c d) )
                ( (list d) )
              ) 
              (f (substr s 2) d "" t)
            )
            (f (substr s 2) d (strcat c k) b)
          )
          (if b (list c) c)
        )
      )
    )
  )

  ;Prompt para o usuario escolher qual esforco calcular;
  (if (null global:resp)
    (setq global:resp "Linear")
  )
  (initget "Linear Angular Resultante")      
  (if (setq tmp-resp (getkword (strcat "\nCalculo de esforco [Linear/Angular/Resultante] <" global:resp ">: ")))
    (setq global:resp tmp-resp)
  )

  (cond 
    ((equal global:resp "Linear")
      (princ "\nSelecione os vaos")
      (if
        (setq suma 0 ht 0 suma2 0 soma-vaos 0 ht2 0 ss (ssget '((0 . "TEXT"))))
        (progn
          (repeat (setq i (sslength ss))
            (setq suma (+ suma (atof (cdr (assoc 1 (setq e (entget (ssname ss (1- i))))))))
                  ht (max ht (cdr (assoc 40 e)))
            )
            (setq suma2 (+ suma2 (expt (atof (cdr (assoc 1 (setq e2 (entget (ssname ss (1- i))))))) 3))
                  ht2 (max ht2 (cdr (assoc 40 e2)))
            )            
            (setq i (1- i))
          )

          (setq soma-vaos (sqrt (/ suma2 suma)))      
          
          ;Prompt para o usuario escolher a quantidade de fibras do cabo optico;
          (if (null global:qnt-fb)
            (setq global:qnt-fb "12F")
          )
          (initget "12F 24F 36F 48F")      
          (if (setq tmp1 (getkword (strcat "\nQuantidade de fibras do cabo optico [12F/24F/36F/48F] <" global:qnt-fb ">: ")))
            (setq global:qnt-fb tmp1)
          )   

          ;Prompt para o usuario escolher o tamanho do poste;
          (if (null global:poste)
            (setq global:poste "11m")
          )
          (initget "9m 11m 12m 13m")      
          (if (setq tmp2 (getkword (strcat "\nAltura do poste [9m/11m/12m/13m] <" global:poste ">: ")))
            (setq global:poste tmp2)
          )    

          ;Prompt para o usuario escolher se possui e qual o tipo de cordoalha;
          (if (null global:cordoalha)
            (setq global:cordoalha "Inexistente")
          )
          (initget "Inexistente Aco Dieletrica")      
          (if (setq tmp3 (getkword (strcat "\nCordoalha [Inexistente/Aco/Dieletrica] <" global:cordoalha ">: ")))
            (setq global:cordoalha tmp3)
          )   

          ;Prompt para o usuario escolher o lado da força;
          (if (null global:lado-forca)
            (setq global:lado-forca "Esquerda")
          )
          (initget "Esquerda Direita")      
          (if (setq tmp4 (getkword (strcat "\nLado da força resultante [Esquerda/Direita] <" global:lado-forca ">: ")))
            (setq global:lado-forca tmp4)
          )           

          (cond 
              ((equal global:qnt-fb "12F")
                ;Calculo da tracao das fibras sem considerar o vento;
                (setq trac12f-sv (/ (* (expt soma-vaos 2) fb12-peso) (* (* 8 soma-vaos) 0.01)))  
                ;Calculo da tracao das fibras considerando o vento;
                (setq trac12f-cv (* 0.00471 (expt 94 2) soma-vaos fb12-dm (cos 45) 0.001))
                ;Cond da altura do poste
                (cond (
                    (equal global:poste "9m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 9m
                        (setq 9m12f-sv (/ (* trac12f-sv 5) 7.3))
                        ;Valor final sem vento
                        (setq final-sv 9m12f-sv)
                      ;Calculo considerando o vento;
                        ;Poste de 9m
                        (setq 9m12f-cv (/ (/ (* trac12f-cv 5) 7.3) 2))      
                        ;Valor final com vento
                        (setq final-cv 9m12f-cv) 
                    )
                    ((equal global:poste "11m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 11m
                        (setq 11m12f-sv (/ (* trac12f-sv 5) 9.1))
                        ;Valor final sem vento
                        (setq final-sv 11m12f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 11m
                        (setq 11m12f-cv (/ (/ (* trac12f-cv 5) 9.1) 2)) 
                        ;Valor final com vento
                        (setq final-cv 11m12f-cv)
                    )
                    ((equal global:poste "12m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 12m
                        (setq 12m12f-sv (/ (* trac12f-sv 5) 10))
                        ;Valor final sem vento
                        (setq final-sv 12m12f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 12m
                        (setq 12m12f-cv (/ (/ (* trac12f-cv 5) 10) 2)) 
                        ;Valor final com vento
                        (setq final-cv 12m12f-cv)
                    )
                    ((equal global:poste "13m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 13m
                        (setq 13m12f-sv (/ (* trac12f-sv 5) 11.2))
                        ;Valor final sem vento
                        (setq final-sv 13m12f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 13m
                        (setq 13m12f-cv (/ (/ (* trac12f-cv 5) 11.2) 2)) 
                        ;Valor final com vento
                        (setq final-cv 13m12f-cv)
                    )                            
                    (t (
                      (princ "\nErro 12F")
                    ))
                )
              )
              ((equal global:qnt-fb "24F")
                ;Calculo da tracao das fibras sem considerar o vento;
                (setq trac24f-sv (/ (* (expt soma-vaos 2) fb24-peso) (* (* 8 soma-vaos) 0.01)))  
                ;Calculo da tracao das fibras considerando o vento;
                (setq trac24f-cv (* 0.00471 (expt 94 2) soma-vaos fb24-dm (cos 45) 0.001))
                ;Cond da altura do poste
                (cond (
                    (equal global:poste "9m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 9m
                        (setq 9m24f-sv (/ (* trac24f-sv 5) 7.3))
                        ;Valor final sem vento
                        (setq final-sv 9m24f-sv)
                      ;Calculo considerando o vento;
                        ;Poste de 9m
                        (setq 9m24f-cv (/ (/ (* trac24f-cv 5) 7.3) 2))      
                        ;Valor final com vento
                        (setq final-cv 9m24f-cv) 
                    )
                    ((equal global:poste "11m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 11m
                        (setq 11m24f-sv (/ (* trac24f-sv 5) 9.1))
                        ;Valor final sem vento
                        (setq final-sv 11m24f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 11m
                        (setq 11m24f-cv (/ (/ (* trac24f-cv 5) 9.1) 2)) 
                        ;Valor final com vento
                        (setq final-cv 11m24f-cv)
                    )
                    ((equal global:poste "12m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 12m
                        (setq 12m24f-sv (/ (* trac24f-sv 5) 10))
                        ;Valor final sem vento
                        (setq final-sv 12m24f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 12m
                        (setq 12m24f-cv (/ (/ (* trac24f-cv 5) 10) 2)) 
                        ;Valor final com vento
                        (setq final-cv 12m24f-cv)
                    )
                    ((equal global:poste "13m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 13m
                        (setq 13m24f-sv (/ (* trac24f-sv 5) 11.2))
                        ;Valor final sem vento
                        (setq final-sv 13m24f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 13m
                        (setq 13m24f-cv (/ (/ (* trac24f-cv 5) 11.2) 2)) 
                        ;Valor final com vento
                        (setq final-cv 13m24f-cv)
                    )                            
                    (t (
                      (princ "\nErro 24F")
                    ))
                )
              )
              ((equal global:qnt-fb "36F")
                ;Calculo da tracao das fibras sem considerar o vento;
                (setq trac36f-sv (/ (* (expt soma-vaos 2) fb36-peso) (* (* 8 soma-vaos) 0.01)))  
                ;Calculo da tracao das fibras considerando o vento;
                (setq trac36f-cv (* 0.00471 (expt 94 2) soma-vaos fb36-dm (cos 45) 0.001))
                ;Cond da altura do poste
                (cond (
                    (equal global:poste "9m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 9m
                        (setq 9m36f-sv (/ (* trac36f-sv 5) 7.3))
                        ;Valor final sem vento
                        (setq final-sv 9m36f-sv)
                      ;Calculo considerando o vento;
                        ;Poste de 9m
                        (setq 9m36f-cv (/ (/ (* trac36f-cv 5) 7.3) 2))      
                        ;Valor final com vento
                        (setq final-cv 9m36f-cv) 
                    )
                    ((equal global:poste "11m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 11m
                        (setq 11m36f-sv (/ (* trac36f-sv 5) 9.1))
                        ;Valor final sem vento
                        (setq final-sv 11m36f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 11m
                        (setq 11m36f-cv (/ (/ (* trac36f-cv 5) 9.1) 2)) 
                        ;Valor final com vento
                        (setq final-cv 11m36f-cv)
                    )
                    ((equal global:poste "12m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 12m
                        (setq 12m36f-sv (/ (* trac36f-sv 5) 10))
                        ;Valor final sem vento
                        (setq final-sv 12m36f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 12m
                        (setq 12m36f-cv (/ (/ (* trac36f-cv 5) 10) 2)) 
                        ;Valor final com vento
                        (setq final-cv 12m36f-cv)
                    )
                    ((equal global:poste "13m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 13m
                        (setq 13m36f-sv (/ (* trac36f-sv 5) 11.2))
                        ;Valor final sem vento
                        (setq final-sv 13m36f-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 13m
                        (setq 13m36f-cv (/ (/ (* trac36f-cv 5) 11.2) 2)) 
                        ;Valor final com vento
                        (setq final-cv 13m36f-cv)
                    )                            
                    (t (
                      (princ "\nErro 36F")
                    ))
                )
              )                     
              (t (
                (princ "\nErro")
              ))
          )      

          ;; Cond para saber qual o tipo de cordoalha foi escolhido
          (cond
              ((equal global:cordoalha "Aco")
                ;Calculo da tracao da cordoalha sem considerar o vento;
                (setq trac-aco-sv (/ (* (expt soma-vaos 2) cord-aco-peso) (* (* 8 soma-vaos) 0.01)))
                ;Calculo da tracao da cordoalha considerando o vento;
                (setq trac-aco-cv (* 0.00471 (expt 94 2) soma-vaos cord-aco-dm (cos 45) 0.001))  
                ;Cond da altura do poste
                (cond (
                    (equal global:poste "9m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 9m
                        (setq 9m-aco-sv (/ (* trac-aco-sv 5) 7.3))
                        ;Valor final sem vento
                        (setq final-cord-sv 9m-aco-sv)
                      ;Calculo considerando o vento;
                        ;Poste de 9m
                        (setq 9m-aco-cv (/ (/ (* trac-aco-cv 5) 7.3) 2))      
                        ;Valor final com vento
                        (setq final-cord-cv 9m-aco-cv) 
                    )
                    ((equal global:poste "11m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 11m
                        (setq 11m-aco-sv (/ (* trac-aco-sv 5) 9.1))
                        ;Valor final sem vento
                        (setq final-cord-sv 11m-aco-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 11m
                        (setq 11m-aco-cv (/ (/ (* trac-aco-cv 5) 9.1) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 11m-aco-cv)
                    )
                    ((equal global:poste "12m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 12m
                        (setq 12m-aco-sv (/ (* trac-aco-sv 5) 10))
                        ;Valor final sem vento
                        (setq final-cord-sv 12m-aco-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 12m
                        (setq 12m-aco-cv (/ (/ (* trac-aco-cv 5) 10) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 12m24f-cv)
                    )
                    ((equal global:poste "13m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 13m
                        (setq 13m-aco-sv (/ (* trac-aco-sv 5) 11.2))
                        ;Valor final sem vento
                        (setq final-cord-sv 13m-aco-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 13m
                        (setq 13m-aco-cv (/ (/ (* trac-aco-cv 5) 11.2) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 13m-aco-cv)
                    )                            
                    (t (
                      (princ "\nErro Aco")
                    ))
                )                      
              )
              ((equal global:cordoalha "Dieletrica")
                ;Calculo da tracao da cordoalha sem considerar o vento;
                (setq trac-dieletrica-sv (/ (* (expt soma-vaos 2) cord-dieletrica-peso) (* (* 8 soma-vaos) 0.01)))
                ;Calculo da tracao da cordoalha considerando o vento;
                (setq trac-dieletrica-cv (* 0.00471 (expt 94 2) soma-vaos cord-dieletrica-dm (cos 45) 0.001))  
                ;Cond da altura do poste
                (cond (
                    (equal global:poste "9m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 9m
                        (setq 9m-dieletrica-sv (/ (* trac-dieletrica-sv 5) 7.3))
                        ;Valor final sem vento
                        (setq final-cord-sv 9m-dieletrica-sv)
                      ;Calculo considerando o vento;
                        ;Poste de 9m
                        (setq 9m-dieletrica-cv (/ (/ (* trac-dieletrica-cv 5) 7.3) 2))      
                        ;Valor final com vento
                        (setq final-cord-cv 9m-dieletrica-cv) 
                    )
                    ((equal global:poste "11m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 11m
                        (setq 11m-dieletrica-sv (/ (* trac-dieletrica-sv 5) 9.1))
                        ;Valor final sem vento
                        (setq final-cord-sv 11m-dieletrica-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 11m
                        (setq 11m-dieletrica-cv (/ (/ (* trac-dieletrica-cv 5) 9.1) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 11m-dieletrica-cv)
                    )
                    ((equal global:poste "12m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 12m
                        (setq 12m-dieletrica-sv (/ (* trac-dieletrica-sv 5) 10))
                        ;Valor final sem vento
                        (setq final-cord-sv 12m-dieletrica-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 12m
                        (setq 12m-dieletrica-cv (/ (/ (* trac-dieletrica-cv 5) 10) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 12m24f-cv)
                    )
                    ((equal global:poste "13m")
                      ;Calculos do cabo - sem considerar o vento;
                        ;Poste de 13m
                        (setq 13m-dieletrica-sv (/ (* trac-dieletrica-sv 5) 11.2))
                        ;Valor final sem vento
                        (setq final-cord-sv 13m-dieletrica-sv)                            
                      ;Calculo considerando o vento;
                        ;Poste de 13m
                        (setq 13m-dieletrica-cv (/ (/ (* trac-dieletrica-cv 5) 11.2) 2)) 
                        ;Valor final com vento
                        (setq final-cord-cv 13m-dieletrica-cv)
                    )                            
                    (t (
                      (princ "\nErro Aco")
                    ))
                )                      
              )          
              ((equal global:cordoalha "Inexistente")
                (setq final-cord-sv 0)
                (setq final-cord-cv 0)
              )          
          )

          ;; Calculo do esforco total
          (setq esforco-fb (sqrt (+ (expt final-sv 2) (expt final-cv 2))))
          (setq esforco-cord (sqrt (+ (expt final-cord-sv 2) (expt final-cord-cv 2))))
          (setq esforco-total (+ esforco-fb esforco-cord))

          (setq strtemp "F\\H0.6x;\\S^R;")
          (setq fmt (strcat "F\\S^R;= " (rtos esforco-total) " kgf"))
          (setq test (subs-string (myrtos esforco-total) "." ","))

          (setq frac (strcat "\\A1;" "F" "{\\H0.70x\\S" "^R;}" "= " (subs-string (myrtos esforco-total) "." ",") " kgf"))

          (if (setq p1 (getpoint "\nPosicao do texto/leader:"))
            (progn
              (setq x (car p1))
              (setq y (cadr p1))
              (setq z (caddr p1))
              (entmake (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbMText") (cons 10 (list x y z))  (cons 40 1.5485) (cons 1 frac)))
              (setq ent-text (entget (entlast)))                   
              (cond 
                ((equal global:lado-forca "Esquerda") 
                  (setq x1 (+ x 0.29))                                   
                  (setq x2 (+ x1 6.8))       
                  (setq y1 (- y 2.6))                                                          
                  (entmake
                    (list
                      '(0 . "LEADER")
                      '(67 . 0)
                      '(410 . "Model")
                      '(100 . "AcDbEntity")
                      '(100 . "AcDbLeader")
                      '(3 . "Standard")                      
                      '(71 . 1)
                      '(72 . 0)
                      '(73 . 3)
                      '(74 . 1)
                      '(75 . 0)
                      '(76 . 1)
                      '(40 . 0.0)
                      '(41 . 0.0)
                      (cons 10 (list x1 y1 z))
                      (cons 10 (list x2 y1 z))                          
                      '(210 0.0 0.0 1.0)
                      '(211 1.0 0.0 0.0)
                      '(212 0.0 0.0 0.0)
                      '(213 0.0 0.0 0.0)
                      '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                    )
                  )  
                  (setq ent-leader (entget (entlast)))
                )
                ((equal global:lado-forca "Direita")
                  (setq x1 (+ x 11.64))                                   
                  (setq x2 (+ x 4.85))       
                  (setq y1 (- y 2.6))                       
                  (entmake
                    (list
                      '(0 . "LEADER")
                      '(67 . 0)
                      '(410 . "Model")
                      '(100 . "AcDbEntity")
                      '(100 . "AcDbLeader")
                      '(3 . "Standard")                      
                      '(71 . 1)
                      '(72 . 0)
                      '(73 . 3)
                      '(74 . 1)
                      '(75 . 0)
                      '(76 . 1)
                      '(40 . 0.0)
                      '(41 . 0.0)
                      (cons 10 (list x1 y1 0.0))
                      (cons 10 (list x2 y1 0.0))                                                
                      '(210 0.0 0.0 1.0)
                      '(211 1.0 0.0 0.0)
                      '(212 0.0 0.0 0.0)
                      '(213 0.0 0.0 0.0)
                      '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                    )  
                ))                  
              
              )
              (setq oss (list ent-text ent-leader))         
              (oa oss (list (+ x 6.14) y z))
            )
          )            

        )
      )  
    )
    ((equal global:resp "Angular")
      (print "Funcao nao implementada ainda")
    )
    ((equal global:resp "Resultante")
      (while
          (progn
              (setvar 'errno 0)
              (setq sel (nentselp "\nSelecione a primeira forca: "))
              (cond
                  (   (= 7 (getvar 'errno))
                      (princ "\nNenhum objeto selecionado, tente novamente.")
                  )
                  (   (= 'ename (type (car sel)))
                      (if
                        (/= "MTEXT" (cdr (assoc 0 (entget (car sel)))))
                          (princ "\nObjeto selecionado invalido.")
                      )
                  )
              )
          )
      )
      (if sel
            (progn
              (setq forc1-str (cdr (assoc 1 (setq e (entget (car sel))))))
              (setq forc1 (atof (subs-string (car (cddr (split forc1-str "="))) "," ".")))
            )
      )  

      (while
          (progn
              (setvar 'errno 0)
              (setq sel2 (nentselp "\nSelecione a segunda forca: "))
                (cond
                    (   (= 7 (getvar 'errno))
                        (princ "\nNenhum objeto selecionado, tente novamente.")
                    )
                    (   (= 'ename (type (car sel2)))
                        (if
                          (/= "MTEXT" (cdr (assoc 0 (entget (car sel2)))))
                            (princ "\nObjeto selecionado invalido.")
                        )
                    )
                )
          )
      )    
      (if sel2
            (progn
              (setq forc2-str (cdr (assoc 1 (setq e (entget (car sel2))))))
              (setq forc2 (atof (subs-string (car (cddr (split forc2-str "="))) "," ".")))
            )
      )              

      (setq result (abs (- forc1 forc2)))       

      ;Prompt para o usuario escolher o lado da força;
      (if (null global:lado-forca)
        (setq global:lado-forca "Esquerda")
      )
      (initget "Esquerda Direita")      
      (if (setq tmp4 (getkword (strcat "\nLado da forca resultante [Esquerda/Direita] <" global:lado-forca ">: ")))
        (setq global:lado-forca tmp4)
      )

      (setq strtemp "F\\H0.6x;\\S^R;")
      (setq fmt (strcat "F\\S^R;= " (rtos result) " kgf"))
      (setq test (subs-string (myrtos result) "." ","))

      (setq frac (strcat "\\A1;" "F" "{\\H0.70x\\S" "^R;}" "= " (subs-string (myrtos result) "." ",") " kgf"))

        (if (setq p1 (getpoint "\nPosicao do texto/leader:"))
          (progn
            (setq x (car p1))
            (setq y (cadr p1))
            (setq z (caddr p1))
            (entmake (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbMText") (cons 10 (list x y z))  (cons 40 1.5485) (cons 1 frac)))
            (setq ent-text (entget (entlast)))                   
            (cond 
              ((equal global:lado-forca "Esquerda") 
                (setq x1 (+ x 0.29))                                   
                (setq x2 (+ x1 6.8))       
                (setq y1 (- y 2.6))                                                          
                (entmake
                  (list
                    '(0 . "LEADER")
                    '(67 . 0)
                    '(410 . "Model")
                    '(100 . "AcDbEntity")
                    '(100 . "AcDbLeader")
                    '(3 . "Standard")                      
                    '(71 . 1)
                    '(72 . 0)
                    '(73 . 3)
                    '(74 . 1)
                    '(75 . 0)
                    '(76 . 1)
                    '(40 . 0.0)
                    '(41 . 0.0)
                    (cons 10 (list x1 y1 z))
                    (cons 10 (list x2 y1 z))                          
                    '(210 0.0 0.0 1.0)
                    '(211 1.0 0.0 0.0)
                    '(212 0.0 0.0 0.0)
                    '(213 0.0 0.0 0.0)
                    '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                  )
                )  
                (setq ent-leader (entget (entlast)))
              )
              ((equal global:lado-forca "Direita")
                (setq x1 (+ x 11.64))                                   
                (setq x2 (+ x 4.85))       
                (setq y1 (- y 2.6))                       
                (entmake
                  (list
                    '(0 . "LEADER")
                    '(67 . 0)
                    '(410 . "Model")
                    '(100 . "AcDbEntity")
                    '(100 . "AcDbLeader")
                    '(3 . "Standard")                      
                    '(71 . 1)
                    '(72 . 0)
                    '(73 . 3)
                    '(74 . 1)
                    '(75 . 0)
                    '(76 . 1)
                    '(40 . 0.0)
                    '(41 . 0.0)
                    (cons 10 (list x1 y1 0.0))
                    (cons 10 (list x2 y1 0.0))                                                
                    '(210 0.0 0.0 1.0)
                    '(211 1.0 0.0 0.0)
                    '(212 0.0 0.0 0.0)
                    '(213 0.0 0.0 0.0)
                    '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                  )  
              ))                              
            )
            (setq oss (list ent-text ent-leader))      
            (oa oss (list (+ x 6.14) y z))
          )
        ) 

    )    
  )

)