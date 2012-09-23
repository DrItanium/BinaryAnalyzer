;------------------------------------------------------------------------------
; Classes.clp - Contains all of the classes used by the expert system portion
; of the analyzer
; Written by Joshua Scoggins (5/31/2012)
;------------------------------------------------------------------------------
(defclass ReflectiveObject (is-a USER)
 (slot Parent (type SYMBOL) (default nil))
 (slot Class (type SYMBOL))
 (slot ID (type SYMBOL)))

(defmessage-handler ReflectiveObject init after ()
 (bind ?self:Class (class ?self))
 (bind ?self:ID (instance-name-to-symbol (instance-name ?self))))

;------------------------------------------------------------------------------
