; AttributedCharacterIterator$Attribute-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.AttributedCharacterIterator$Attribute*
 (make-class-def
      '(class "java.text.AttributedCharacterIterator$Attribute"
            "java.lang.Object"
            (constant_pool
                        (LONG -9142742483513960612)
                        (STRING  "(")
                        (STRING  ")")
                        (STRING  "subclass didn\nt correctly implement readResolve")
                        (STRING  "unknown attribute name")
                        (STRING  "language")
                        (STRING  "reading")
                        (STRING  "input_method_segment"))
            (fields
                        (field "name" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "instanceMap" (class "java.util.Map") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "LANGUAGE" (class "java.text.AttributedCharacterIterator$Attribute") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "READING" (class "java.text.AttributedCharacterIterator$Attribute") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "INPUT_METHOD_SEGMENT" (class "java.text.AttributedCharacterIterator$Attribute") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (putfield (fieldCP "name" "java.text.AttributedCharacterIterator$Attribute" (class "java.lang.String")))) 
                                      (9 (aload_0)) 
                                      (10 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (13 (ldc_w )) 
                                      (16 (if_acmpne 30))  ;;to TAG_0
                                      (19 (getstatic (fieldCP "instanceMap" "java.text.AttributedCharacterIterator$Attribute" (class "java.util.Map")))) 
                                      (22 (aload_1)) 
                                      (23 (aload_0)) 
                                      (24 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (29 (pop)) 
                                      (30 (return)) ;;at TAG_0
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "hashCode" "java.lang.Object" () int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (11 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (17 (ldc 1))        ;;STRING:: "("
                                      (19 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "name" "java.text.AttributedCharacterIterator$Attribute" (class "java.lang.String"))))
                                      (26 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (29 (ldc 2))        ;;STRING:: ")"
                                      (31 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (34 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.text.AttributedCharacterIterator$Attribute" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readResolve"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (4 (ldc_w )) 
                                      (7 (if_acmpeq 20))  ;;to TAG_0
                                      (10 (new (class "java.io.InvalidObjectException"))) 
                                      (13 (dup)) 
                                      (14 (ldc 3)) ;;STRING:: "subclass didn\nt correctly implement readResolve"
                                      (16 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (19 (athrow)) 
                                      (20 (getstatic (fieldCP "instanceMap" "java.text.AttributedCharacterIterator$Attribute" (class "java.util.Map")))) ;;at TAG_0
                                      (23 (aload_0)) 
                                      (24 (invokevirtual (methodCP "getName" "java.text.AttributedCharacterIterator$Attribute" () (class "java.lang.String")))) 
                                      (27 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (32 (checkcast (class "java.text.AttributedCharacterIterator$Attribute"))) 
                                      (35 (astore_1)) 
                                      (36 (aload_1)) 
                                      (37 (ifnull 42)) ;;to TAG_1
                                      (40 (aload_1)) 
                                      (41 (areturn)) 
                                      (42 (new (class "java.io.InvalidObjectException"))) ;;at TAG_1
                                      (45 (dup)) 
                                      (46 (ldc 4)) ;;STRING:: "unknown attribute name"
                                      (48 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (51 (athrow)) 
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 49)
                                   (parsedcode
                                      (0 (new (class "java.util.HashMap")))
                                      (3 (dup))
                                      (4 (bipush 7))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.HashMap" (int) void)))
                                      (9 (putstatic (fieldCP "instanceMap" "java.text.AttributedCharacterIterator$Attribute" (class "java.util.Map"))))
                                      (12 (new (class "java.text.AttributedCharacterIterator$Attribute")))
                                      (15 (dup))
                                      (16 (ldc 5))        ;;STRING:: "language"
                                      (18 (invokespecial
					(methodCP "<init>" "java.text.AttributedCharacterIterator$Attribute" ((class "java.lang.String")) void)))
                                      (21 (putstatic (fieldCP "LANGUAGE" "java.text.AttributedCharacterIterator$Attribute" (class "java.text.AttributedCharacterIterator$Attribute"))))
                                      (24 (new (class "java.text.AttributedCharacterIterator$Attribute")))
                                      (27 (dup))
                                      (28 (ldc 6))        ;;STRING:: "reading"
                                      (30 (invokespecial
					(methodCP "<init>" "java.text.AttributedCharacterIterator$Attribute" ((class "java.lang.String")) void)))
                                      (33 (putstatic (fieldCP "READING" "java.text.AttributedCharacterIterator$Attribute" (class "java.text.AttributedCharacterIterator$Attribute"))))
                                      (36 (new (class "java.text.AttributedCharacterIterator$Attribute")))
                                      (39 (dup))
                                      (40 (ldc 7))        ;;STRING:: "input_method_segment"
                                      (42 (invokespecial
					(methodCP "<init>" "java.text.AttributedCharacterIterator$Attribute" ((class "java.lang.String")) void)))
                                      (45 (putstatic (fieldCP "INPUT_METHOD_SEGMENT" "java.text.AttributedCharacterIterator$Attribute" (class "java.text.AttributedCharacterIterator$Attribute"))))
                                      (48 (return))
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *AttributedCharacterIterator$Attribute-class-table*
  (make-static-class-decls 
   *java.text.AttributedCharacterIterator$Attribute*))

(defconst *package-name-map* 
  ("java.text.AttributedCharacterIterator$Attribute" . "java.text"))
