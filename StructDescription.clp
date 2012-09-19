(defclass StructField "Represents a single field in a given C struct"
 (is-a ReflectiveObject)
 (slot Name (type SYMBOL))
 (slot Width (type NUMBER) (range 0 ?VARIABLE) (default 0))
 (slot Order (type NUMBER) (range -1 ?VARIABLE) (default -1 ))
 (slot IsArray (type SYMBOL) (allowed-values FALSE TRUE))
 (slot IsPointer (type SYMBOL) (allowed-values FALSE TRUE))
 (slot Value))

(defclass StructDescription 
 "The idea behind this super type is that the programmer
 provides a list of fields to the given multislot and the
 expert system is then responsible for attempting to bind
 different values to different fields"
 (is-a ReflectiveObject)
 (slot StructName (type SYMBOL))
 (multislot Fields (type INSTANCE_ADDRESS INSTANCE_NAME) (allowed-classes
                                                          StructField)))

