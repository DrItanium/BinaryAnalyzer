; To run this call clips -f Test.clp
; then at the REPL: (run)
(load* "Classes.clp")
(load* "Rules.clp")
;(definstances Test
; ((gensym*) of File (Parent a0) (FileContents 0 1 2 3 4 5 6 7 8))
; ((gensym*) of File (Parent a0) (FileContents 0 1 2 3 4 8 6 7 5))
; ((gensym*) of File (Parent a0) (FileContents 0 1 2 3 4 5 6 7 8 9 10))
; ((gensym*) of File (Parent a0) (FileContents 9 8 7 6 5 4 3 2 1 0 11))
; ((gensym*) of File (Parent a0) (FileContents (random) (random) (random)
;	 (random) (random))))
;(reset)
(load-file "Characters\Midnight")
(load-file "Characters\Cindred")
