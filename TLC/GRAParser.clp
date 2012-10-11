; GRAParser.clp - Contains rules and classes to parse GRA files found in the
; educational game Midnight Rescue by The Learning Company
; Thanks to http://www.shikadi.net/moddingwiki/PAK_Format_(The_Learning_Company)
; For influence in reading these damn files. The link doesn't exactly describe
; the information but it did get me started.
; -----------------------------------------------------------------------------
; The learning company uses a very interesting series of file formats. For
; instance, they have DAT, RSC, and GRA files in addition to the EXE itself.
; 
; The GRA format that they use it also very strange in that the table
; describing the list of graphics contained in the file is delimited
; differently. For instance, in DOPEN.GRA in Midnight Rescue! each entry is
; delimited by six bytes with a value of 0. In DATTRIB.GRA each entry is
; delimited by two bytes with a value of 0. This makes the formats not only
; application specific but designed to minimize space usage. This makes it
; harder for me because I have to figure out a safe way to safely read these
; files regardless of the padding....

(defclass GRAFile (is-a File)
 "Represents the table that describes a series of images contained in a
  GRA file for the game Midnight Resucue by The Learning Company"
 (slot Parsed (type SYMBOL) (allowed-symbols FALSE TRUE))
 (slot Size (type NUMBER) (range 0 ?VARIABLE))
 (multislot TableEntries))

(defclass GRAEntry (is-a ReflectiveObject)
 (slot Index (type NUMBER))
 (slot Offset (type NUMBER))
 (slot Identifier (type NUMBER) (range 0 ?VARIABLE))
 (slot Length (type NUMBER) (range 0 ?VARIABLE))
 (slot Height (type NUMBER) (range 0 ?VARIABLE))
 ; As I continue to reverse engineer this file format I will rename
 ; these fields
 (slot Unknown0 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown1 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown2 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown3 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown4 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown5 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown6 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown7 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown8 (type NUMBER) (range 0 ?VARIABLE))
 (slot Unknown9 (type NUMBER) (range 0 ?VARIABLE))
 (slot Terminate (type NUMBER) (range 0 ?VARIABLE))
 (multislot Rest (type NUMBER)))

(deffunction load-gra-file (?name ?path) (load-file ?name ?path gra))


(defrule LoadGRAFile
 ?f <- (Load file named ?name from path ?path type gra)
 (not (exists (object (is-a MZFile) (ID ?name))))
 =>
 (retract ?f)
 (bind ?logicalName (gensym*))
 (open ?path ?logicalName)
 (make-instance ?name of GRAFile (Path ?path) (FileHandleName ?logicalName)))

(defrule ParseGRAHeader 
 ?g <- (object (is-a GRAFile) (Parsed FALSE) (ID ?id))
 =>
 (bind ?size (send ?g .ReadLittleEndianShort))
 (bind ?j 0)
 ;build the entries
 (while (< ?j ?size) do
  (bind ?objName (sym-cat gra-entry- ?id - ?j)) 
  ;TODO: Continue this code while I reverse the file format
  (make-instance ?objName of GRAEntry 
	(Index ?j)))
 (modify-instance ?g (Parsed TRUE) (Size ?size))
 


