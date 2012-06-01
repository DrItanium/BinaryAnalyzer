;------------------------------------------------------------------------------
; Functions.clp - Contains helper functions 
; Written by Joshua Scoggins (5/31/2012)
;------------------------------------------------------------------------------
(deffunction load-file-contents (?path ?logicalName)
 (open ?path ?logicalName)
 (bind ?tmp (get-char ?logicalName))
 (bind ?contents (create$ ?tmp))
 (while (neq ?tmp -1) do 
 (bind ?contents (create$ ?contents ?tmp))
 (bind ?tmp (get-char ?logicalName)))
 (close tmpFile)
 (return ?contents))
;------------------------------------------------------------------------------
