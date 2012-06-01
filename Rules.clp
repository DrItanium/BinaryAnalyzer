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
(defrule AssertDifferenceInLength
 "Checks to see if two files of the same group have different total lengths"
 (declare (salience 1))
 ?file0 <- (object (is-a BinaryFile) (ID ?id) (Parent ?p) (FileContents $?a)) 
 ?file1 <- (object (is-a BinaryFile) (ID ~?id) (Parent ?p) (FileContents $?b))
 (test (neq (length ?a) (length ?b)))
 =>
 (bind ?la (length ?a))
 (bind ?lb (length ?b))
 (if (> ?la ?lb) then
  (bind ?diff (- ?la ?lb))
  (assert (File (send ?file0 get-ID) longer than (send ?file1 get-ID) by ?diff))
  else
  (bind ?diff (- ?lb ?la))
  (assert (File (send ?file1 get-ID) longer than (send ?file0 get-ID) by ?diff))))

(defrule MarkDifference
 "Takes two different files and compares their contents. It will only do this
 on files within the same group. This group is defined in the Parent field"
 (declare (salience 1))
 (object (is-a BinaryFile) (ID ?id) (Parent ?p) (FileContents $?a ?curr $?))
 (object (is-a BinaryFile) (ID ?id1&~?id) (Parent ?p) (FileContents $?b  ?curr2&~?curr $?))
 (test (eq (length ?a) (length ?b)))
 =>
 (assert (Difference ?id ?id1 Bytes ?curr ?curr2)))

(defrule PrintoutFoundDifference
 ?diff <- (Difference ?f0 ?f1 Bytes ?c0 ?c1)
 =>
 (retract ?diff)
 (printout t "In files " ?f0 " and " ?f1 " bytes " ?c0 " and " ?c1 
  " are different between the respective files" crlf))

(defrule PrintoutLengthDifference
 ?diff <- (File ?longer longer than ?shorter by ?difference)
 =>
 (retract ?diff)
 (printout t "File " ?longer " is longer than " ?shorter " by " ?difference 
  " bytes" crlf))