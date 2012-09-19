
(defclass MachineDescription (is-a ReflectiveObject)
 (slot MachineName (type SYMBOL STRING))
 (slot IntSize (type NUMBER) (range 0 ?VARIABLE))
 (slot LongSize (type NUMBER) (range 0 ?VARIABLE))
 (slot NullTerminatedStrings (type SYMBOL) (allowed-values TRUE FALSE))
 
 )
