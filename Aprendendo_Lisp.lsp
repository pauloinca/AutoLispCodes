(defun C:CalcEsforco(/ *error* dest i e ss ht suma enti oldzin pct prec val p1 suma2 a ht2 e2 somavaos)
  ;(setq t1(getreal "\nNota teste-1:"))
  ;(setq t2(getreal "\nNota teste-2:"))
  ;(setq nota(soma-e-divide))
  (princ "\nSelecione os vaos")

  ;Definicao de constantes

    ;Peso de cada fibra - Sterlite
    (setq fb12-peso 0.049) ;Cabo de 12 fibras ;;preciso ajustar
    (setq fb24-peso 0.069) ;Cabo de 24 fibras
    (setq fb36-peso 0.069) ;Cabo de 36 fibras

    ;Diametro de cada fibra - Sterlite
    (setq fb12-dm 8) ; Cabo de 12 fibras ;; preciso ajustar
    (setq fb24-dm 9.8) ; Cabo de 24 fibras
    (setq fb36-dm 9.8) ; Cabo de 36 fibras

    ;Velocidade do vento em Km/h
    (setq vel-vent 94)

    ;Dados da cordoalha dieletrica(1/4 - FRP) - Fibracem
    (setq cord-dieletrica-peso 0.043)
    (setq cord-dieletrica-dm 6.35)

    ;Dados da cordoalha de aco(3/16 - 7 fios) - Eletro Luminar
    (setq cord-aco-peso 0.108)
    (setq cord-aco-dm 4.76)

  (defun *error* (msg)
    (and msg (/= msg "Function cancelled") (princ msg))
  )

  (defun myrtos ( x / dim rtn )
    (setq dim (getvar 'dimzin))
    (setvar 'dimzin 4)
    (setq rtn (vl-catch-all-apply 'rtos (list x 2 (if (< x 1) 2 1))))
    (setvar 'dimzin dim)
    (if (not (vl-catch-all-error-p rtn)) rtn)
  )

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

      (setq esforco-fb (sqrt (+ (expt final-sv 2) (expt final-cv 2))))
      (setq esforco-cord (sqrt (+ (expt final-cord-sv 2) (expt final-cord-cv 2))))
      (setq esforco-total (+ esforco-fb esforco-cord))

      (setq strtemp "F\\H0.6x;\\S^R;")
      (setq fmt (strcat "F\\S^R;= " (rtos esforco-total) " kgf"))
      (setq test (subs-string (myrtos esforco-total) "." ","))

      (setq frac (strcat "\\A1;" "F" "{\\H0.70x\\S" "^R;}" "= " (subs-string (myrtos esforco-total) "." ",") " kgf"))

       (princ "\nSelect Existing Text Entity to be replaced OR Hit Enter Twice To Place as New text")
      ;;  (setq dest (ssget ":E:S:L" '((0 . "TEXT"))))
      ;;  (setq wait 1)
      ;;  (cond ( (wait)
              ;; (setq enti (vlax-ename->vla-object (ssname dest 0))
              ;;       val  (vla-get-textstring enti)
              ;;       )
              ;; (if (setq pct (vl-string-search "." val)) (setq prec (- (strlen val) pct 1)) (setq prec 0))
              ;; (if (> prec 0) (setvar "DIMZIN" 1) (setvar "DIMZIN" 8) )
              ;; (vla-put-textstring enti (rtos esforco-total 2 prec))
              ;; )
            ;;  ((setq p1 (getpoint "\nText Position:"))
            ;;   (entmake (list '(0 . "TEXT") (cons 10 p1) (cons 40 1.5485) (cons 1 "bac")))
            ;;  )
             (if (setq p1 (getpoint "\nText Position:"))
                (progn
                  (setq x (car p1))
                  (setq y (cadr p1))
                  (setq z (caddr p1))
                  (print (strcat "x: " (rtos x)))
                  (print (strcat "y: " (rtos y)))
                  ;; (princ (strcat "\ny: " (y) "\n"))
                  ;; (princ (strcat "\nz: " (z) "\n"))
                  (entmake (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbMText") (cons 10 (list x y z))  (cons 40 1.5485) (cons 1 frac)))
                  (cond 
                    ((equal global:lado-forca "Esquerda")(                                       
                      (setq x2 (+ x 6.8))       
                      (setq y1 (- y 2.5))  
                      (print (strcat "x2: " (rtos x2)))
                      (print (strcat "y1: " (rtos y1)))
                      ;; (princ (strcat "\nx2: " x2 "\n"))
                      ;; (princ (strcat "\ny1: " y1 "\n"))                                                             
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
                          (cons 10 (list x y1 z))
                          (cons 10 (list x2 y1 z))
                          ;; (cons 10 (list x3 y1 0.0))
                          '(210 0.0 0.0 1.0)
                          '(211 1.0 0.0 0.0)
                          '(212 0.0 0.0 0.0)
                          '(213 0.0 0.0 0.0)
                          '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                        )
                      )  
                    ))
                    ((equal global:lado-forca "Direita")(
                      (setq y1 (- y 2.5))
                      (setq x2 (+ x 6.8))       
                      (princ "\n y: ")(princ y)(princ "\n")
                      (princ "\n x2: ")(princ x2)(princ "\n")
                      (princ "\n y1: ")(princ y1)(princ "\n")
                      (princ "\n z: ")(princ z)(princ "\n")                   
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
                          (cons 10 (list x y1 0.0))
                          (cons 10 (list x2 y1 0.0))
                          ;; (cons 10 (list x3 y1 0.0))                        
                          '(210 0.0 0.0 1.0)
                          '(211 1.0 0.0 0.0)
                          '(212 0.0 0.0 0.0)
                          '(213 0.0 0.0 0.0)
                          '(-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 41) (1040 . 2.0) (1070 . 147) (1040 . 0.09) (1070 . 77) (1070 . 0) (1002 . "}")))                      
                        ))  
                    ))                  
                  
                  )              
                )
              )            

    )
  )
)