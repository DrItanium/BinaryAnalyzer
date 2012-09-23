; File.clp - Contains classes, messagehandlers, and rules to automate the
; process of handling the loading of files

(defclass File (is-a MetadataTag)
 (multislot FileContents (type SYMBOL NUMBER))
 (slot FileHandleName (type SYMBOL))
 (slot Path (type STRING))
 (slot Length (type NUMBER)) 
 (message-handler .ReadChar)
 (message-handler .ReadSeveralChars)
 (message-handler .ReadLittleEndianShort)
 (message-handler .ReadBigEndianShort))

(defclass MZFile (is-a File)
 (slot BytesInLastBlock (type NUMBER))
 (slot BlocksInFile (type NUMBER))
 (slot NumRelocs (type NUMBER))
 (slot HeaderParagraphs (type NUMBER))
 (slot MinExtraParagraphs (type NUMBER))
 (slot MaxExtraParagraphs (type NUMBER))
 (slot SS (type NUMBER))
 (slot SP (type NUMBER))
 (slot Checksum (type NUMBER))
 (slot IP (type NUMBER))
 (slot CS (type NUMBER))
 (slot RelocTableOffset (type NUMBER))
 (slot OverlayNumber (type NUMBER))
 (slot StartOfData (type NUMBER))
 (slot ExtraDataStart (type NUMBER)))

(defmessage-handler File delete before ()
 (close ?self:FileHandleName))

(defmessage-handler File .ReadChar ()
 (bind ?result (get-char ?self:FileHandleName))
 (bind ?newSize (+ ?self:Length 1))
 (if (neq -1 ?result) then
   (slot-direct-insert$ FileContents ?newSize ?result)
   (bind ?self:Length ?newSize))
 (return ?result))

(defmessage-handler File .ReadSeveralChars (?count)
  (bind ?result (create$))
  (bind ?oldSize (+ ?self:Length 1))
  (bind ?newSize ?self:Length)
  (bind ?i ?count)
  (while (> ?i 0) do
	(bind ?tmp (get-char ?self:FileHandleName))
	(if (neq ?tmp -1) then
	 (bind ?newSize (+ ?newSize 1))
	 (bind ?result (create$ ?result ?tmp))
	 (bind ?i (- ?i 1))
	 else
	 (break)))
  (slot-direct-insert$ FileContents ?oldSize ?result)
  (bind ?self:Length ?newSize)
  (return ?result))
	 
(defmessage-handler File .ReadLittleEndianShort ()
 (bind ?result (send ?self .ReadSeveralChars 2))
 (return (+ (nth$ 1 ?result)
			 (* (nth$ 2 ?result) 256))))

(defmessage-handler File .ReadBigEndianShort ()
 (bind ?result (send ?self .ReadSeveralChars 2))
 (return (+ (nth$ 2 ?result)
			 (* (nth$ 1 ?result) 256))))

(deffunction load-file (?name ?path ?type)
 (assert (Load file named ?name from path ?path type ?type)))

(deffunction load-generic-file (?name ?path) (load-file ?name ?path generic))
(deffunction load-mz-file (?name ?path) (load-file ?name ?path mz))

(defrule LoadGenericFile 
 ?f0 <- (Load file named ?name from path ?path type generic)
 (not (exists (object (is-a File) (ID ?name))))
 =>
 (retract ?f0)
 (bind ?logicalName (gensym*))
 (open ?path ?logicalName)
 (make-instance ?name of File (Path ?path) (FileHandleName ?logicalName)))

(defrule RetractAlreadyLoadedGenericFile
 ?f0 <- (Load file named ?name from path ?path type generic)
 (exists (object (is-a File) (ID ?name)))
 =>
 (retract ?f0))

(defrule LoadMZFile 
 ?f0 <- (Load file named ?name from path ?path type mz)
 (not (exists (object (is-a MZFile) (ID ?name))))
 =>
 (retract ?f0)
 (bind ?logicalName (gensym*))
 (open ?path ?logicalName)
 (make-instance ?name of MZFile (Path ?path) (FileHandleName ?logicalName)))

(defrule RetractAlreadyLoadedMZFile
 ?f0 <- (Load file named ?name from path ?path type mz)
 (exists (object (is-a MZFile) (ID ?name)))
 =>
 (retract ?f0))

(defrule ComputeMZHeader
 ;28
 ?m <- (object (is-a MZFile) (Length 0) (ID ?id))
 =>
 ;Signature
 (send ?m .ReadLittleEndianShort)
 (modify-instance ?m 
 (BytesInLastBlock (send ?m .ReadLittleEndianShort))
 (BlocksInFile (send ?m .ReadLittleEndianShort))
 (NumRelocs (send ?m .ReadLittleEndianShort))
 (HeaderParagraphs (send ?m .ReadLittleEndianShort))
 (MinExtraParagraphs (send ?m .ReadLittleEndianShort))
 (MaxExtraParagraphs (send ?m .ReadLittleEndianShort))
 (SS (send ?m .ReadLittleEndianShort))
 (SP (send ?m .ReadLittleEndianShort))
 (Checksum (send ?m .ReadLittleEndianShort))
 (IP (send ?m .ReadLittleEndianShort))
 (CS (send ?m .ReadLittleEndianShort))
 (RelocTableOffset (send ?m .ReadLittleEndianShort))
 (OverlayNumber (send ?m .ReadLittleEndianShort)))
 (assert (Compute data start for ?id)))


(defrule ComputeExeDataStart
 ?fct <- (Compute data start for ?id)
 ?m <- (object (is-a MZFile) 
	 				(ID ?id) (Length 28)
	            (HeaderParagraphs ?z))
 =>
 (modify-instance ?m (StartOfData (* 16 ?z))))
