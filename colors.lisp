(in-package :sqnc)

; colors

(sequential-constants 0
  (ma [syma<b _ 0 22] '(grey red green blue)))

(sequential-constants 1
  '(w-gr4 w-gr5 w-gr6 w-gr7 w-gr8 w-gr9 gr1-gr9 gr8-gr4
    gr2-gr4 gr2-gr6 gr2-gr8 gr3-gr9 gr4-gr8 gr5-gr8
    gr5-gr9 gr4-gr6 gr6-gr4 gr9-gr3 gr8-gr5 gr2-gr5
    w-r1 w-r4 w-b0 w-b6 w-b9))

(plurals init-color colors-init 4
         init-pair  init-pairs  3)

(mac init-colors ()
  `(pn (start-color)
       (colors-init ,@(loop for i below 10
                         for n = (* (- 10 i) 100)
                         append `(,(mksym 'grey  i) ,n ,n ,n
                                  ,(mksym 'red   i) ,n  0  0
                                  ,(mksym 'green i)  0 ,n  0
                                  ,(mksym 'blue  i)  0  0 ,n)))
       (init-pairs w-gr4    grey0  grey4
                   w-gr5    grey0  grey5
                   w-gr6    grey0  grey6
                   w-gr7    grey0  grey7
                   w-gr8    grey0  grey8
                   w-gr9    grey0  grey9
                   gr1-gr9  grey1  grey9
                   gr8-gr4  grey8  grey4
                   gr2-gr4  grey2  grey4
                   gr2-gr6  grey2  grey6
                   gr2-gr8  grey2  grey8
                   gr3-gr9  grey3  grey9
                   gr4-gr6  grey4  grey6
                   gr4-gr8  grey4  grey8
                   gr5-gr8  grey5  grey8
                   gr5-gr9  grey5  grey9
                   gr6-gr4  grey6  grey4
                   gr9-gr3  grey9  grey3
                   gr8-gr5  grey8  grey5
                   gr2-gr5  grey2  grey5
                   w-r1     grey0  red1
                   w-r4     grey0  red4
                   w-b0     grey0  blue0
                   w-b6     grey0  blue6
                   w-b9     grey0  blue9)))
