;------------------------------------------------------------------------------
; Rules.clp - Contains all of the rules defined to do binary diffs
; Written by Joshua Scoggins (5/31/2012)
;------------------------------------------------------------------------------
; Assumptions to make about this system
; 1) The order of the elements within the FileContents is important
; 2) The analyzer operates on bytes.
; 3) The analyzer doesn't know anything about the actual content of the file.
;    It is up to the programmer to provide that knowledge.
;------------------------------------------------------------------------------

(defrule MarkDifference
 "Takes two different files and compares their contents. It will only do this
 on files within the same group. This group is defined in the Parent field"
 ?file0 <- (object (is-a BinaryFile) (ID ?id) (Parent ?p) (FileContents $? ?curr $?))
 ?file1 <- (object (is-a BinaryFile) (ID ~?id) (Parent ?p) (FileContents $?  ?curr2&~?curr $?))
 =>
 (assert (Difference ?file0 ?file1 Bytes ?curr ?curr2)))

(defrule PrintoutFoundDifference
 ?diff <- (Difference ?f0 ?f1 Bytes ?c0 ?c1)
 =>
 (retract ?diff)
 (printout t "In files " ?f0 " and " ?f1 " bytes " ?c0 " and " ?c1 
  " are different between the respective files" crlf))
