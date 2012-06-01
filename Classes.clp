;------------------------------------------------------------------------------
; Classes.clp - Contains all of the classes used by the expert system portion
; of the analyzer
; Written by Joshua Scoggins (5/31/2012)
;------------------------------------------------------------------------------
(defclass ReflectiveObject (is-a USER)
 (slot Parent (type SYMBOL) (default nil))
 (slot Type (type SYMBOL))
 (slot ID (type SYMBOL)))

(defmessage-handler ReflectiveObject init after ()
 (bind ?self:Type (class ?self))
 (bind ?self:ID (instance-name-to-symbol (instance-name ?self))))

;------------------------------------------------------------------------------
(defclass File (is-a ReflectiveObject)
 (multislot FileContents (type NUMBER SYMBOL)))

(deffunction load-file (?path)
 (open ?path tmpFile)
 (bind ?obj (make-instance (gensym*) of File))
 (bind ?tmp (get-char tmpFile))
 (bind ?contents (create$ ?tmp))
 (while (neq ?tmp -1) do 
 (bind ?contents (create$ ?contents ?tmp))
 (bind ?tmp (get-char tmpFile)))
 (send ?obj put-FileContents ?contents)
 (close tmpFile)
 (return ?obj))
;------------------------------------------------------------------------------

