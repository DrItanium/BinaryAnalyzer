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
 (assert (Make file from (load-file-contents ?path (gensym*)))))
;------------------------------------------------------------------------------

(defrule MakeFileObject 
 ?f <- (Make file from $?bytes)
 =>
 (retract ?f)
 (make-instance of File (FileContents $?bytes)))

