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
 (declare (salience 2))
 ?file0 <- (object (is-a File) (ID ?id) (Parent ?p) (FileContents $?a)) 
 ?file1 <- (object (is-a File) (ID ~?id) (Parent ?p) (FileContents $?b))
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
;------------------------------------------------------------------------------
(defrule MarkDifference
 "Takes two different files and compares their contents. It will only do this
 on files within the same group. This group is defined in the Parent field"
 (declare (salience 2))
 (object (is-a File) (ID ?id) (Parent ?p) (FileContents $?a ?curr $?))
 (object (is-a File) (ID ?id1&~?id) (Parent ?p) (FileContents $?b  ?curr2&~?curr $?))
 (test (eq (length ?a) (length ?b)))
 =>
 (assert (Difference ?id ?id1 Bytes ?curr ?curr2 at (length ?a))))
;------------------------------------------------------------------------------
(defrule PrintoutFoundDifference
 ?diff <- (Difference ?f0 ?f1 Bytes ?c0 ?c1 at ?ind)
 =>
 (retract ?diff)
 (printout t "In files " ?f0 " and " ?f1 " bytes " ?c0 " and " ?c1 
  " are different between the respective files at position " ?ind crlf))
;------------------------------------------------------------------------------
(defrule PrintoutLengthDifference
 ?diff <- (File ?longer longer than ?shorter by ?difference)
 =>
 (retract ?diff)
 (printout t "File " ?longer " is longer than " ?shorter " by " ?difference 
  " bytes" crlf))
;------------------------------------------------------------------------------
(defrule IdentifyTransposition
 "Does a check to see if two difference facts are actually describing a
 potential transposition"
 (declare (salience 1))
 ?fact0 <- (Difference ?id0 ?id1 Bytes ?c0 ?c1 at ?l)
 ?fact1 <- (Difference ?id1 ?id0 Bytes ?c1 ?c0 at ?l)
 =>
 (retract ?fact0 ?fact1)
 (assert (Transposed ?id0 ?id1 Bytes ?c0 ?c1 at ?l)))
;------------------------------------------------------------------------------
(defrule PrintoutFoundTransposition
 ?diff <- (Transposed ?f0 ?f1 Bytes ?c0 ?c1 at ?ind)
 =>
 (retract ?diff)
 (printout t "Files " ?f0 " and " ?f1 " have different values at position "
  ?ind " being " ?c0 " and " ?c1 " respectively." crlf))
;------------------------------------------------------------------------------
(defrule MarkSimilarity 
 "Takes two different files and compares their contents. It will only do this
 on files within the same group. This group is defined in the Parent field"
 (declare (salience 2))
 (object (is-a File) (ID ?id) (Parent ?p) (FileContents $?a ?curr $?))
 (object (is-a File) (ID ?id1&~?id) (Parent ?p) (FileContents $?b  ?curr $?))
 (test (eq (length ?a) (length ?b)))
 =>
 (assert (Commonality ?id ?id1 Byte ?curr at (length ?a))))
;------------------------------------------------------------------------------
(defrule MergeSimilarities
 "Merges similarities if they are only different by transposed files. This has
 the side effect of reducing the amount of redundant information given to the
 programmer at the end of execution"
 (declare (salience 1))
 ?c0 <- (Commonality ?id0 ?id1 Byte ?curr at ?l)
 ?c1 <- (Commonality ?id1 ?id0 Byte ?curr at ?l)
 =>
 (retract ?c0 ?c1)
 (assert (Commonality ?id0 ?id1 Byte ?curr at ?l)))
;------------------------------------------------------------------------------
(defrule PrintoutSimilarities
 ?diff <- (Commonality ?id0 ?id1 Byte ?curr at ?len)
 =>
 (retract ?diff)
 (printout t "The byte " ?curr " is the same in files " ?id0 " and " ?id1 
  " at position " ?len crlf))
;------------------------------------------------------------------------------
