; To run this call clips -f Test.clp
; then at the REPL: (run)
(load* "Functions.clp")
(load* "Classes.clp")
(load* "MetadataTag.clp")
(load* "File.clp")
(load* "Rules.clp")
(load* "SystemCommands.clp")
;WARNING THIS IS REALLY SLOW 
; I'm not sure why though :(
;(load-file "Characters/Midnight")
;(load-file "Characters/Cindred")
;(load-file "Characters/Sliver")
;(load-file "Characters/Wyrmwood")
;(load-file "Characters/abcdef")
;(load-file "Characters/abcdef.bak")
(load-mz-file underworld "/home/jwscoggins/uw/uw.exe")
