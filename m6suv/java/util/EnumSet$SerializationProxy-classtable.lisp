; EnumSet$SerializationProxy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EnumSet$SerializationProxy*
 (make-class-def
      '(class "java.util.EnumSet$SerializationProxy"
            "java.lang.Object"
            (constant_pool
                        (LONG 362491234563181265))
            (fields
                        (field "elementType" (class "java.lang.Class") (accessflags  *class*  *final*  *private* ) -1)
                        (field "elements" (array (class "java.lang.Enum")) (accessflags  *class*  *final*  *private* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.EnumSet"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (getfield (fieldCP "elementType" "java.util.EnumSet" (class "java.lang.Class"))))
                                      (9 (putfield (fieldCP "elementType" "java.util.EnumSet$SerializationProxy" (class "java.lang.Class"))))
                                      (12 (aload_0))
                                      (13 (aload_1))
                                      (14 (invokestatic
					(methodCP "access$000" "java.util.EnumSet" () (array (class "java.lang.Enum")))))
                                      (17 (invokevirtual
					(methodCP "toArray" "java.util.EnumSet" ((array (class "java.lang.Object"))) (array (class "java.lang.Object")))))
                                      (20 (checkcast (array (class "java.lang.Enum"))))
                                      (23 (putfield (fieldCP "elements" "java.util.EnumSet$SerializationProxy" (array (class "java.lang.Enum")))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readResolve"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "elementType" "java.util.EnumSet$SerializationProxy" (class "java.lang.Class")))) 
                                      (4 (invokestatic (methodCP "noneOf" "java.util.EnumSet" ((class "java.lang.Class")) (class "java.util.EnumSet")))) 
                                      (7 (astore_1)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "elements" "java.util.EnumSet$SerializationProxy" (array (class "java.lang.Enum"))))) 
                                      (12 (astore_2)) 
                                      (13 (aload_2)) 
                                      (14 (arraylength)) 
                                      (15 (istore_3)) 
                                      (16 (iconst_0)) 
                                      (17 (istore 4)) 
                                      (19 (iload 4)) ;;at TAG_1
                                      (21 (iload_3)) 
                                      (22 (if_icmpge 44))  ;;to TAG_0
                                      (25 (aload_2)) 
                                      (26 (iload 4)) 
                                      (28 (aaload)) 
                                      (29 (astore 5)) 
                                      (31 (aload_1)) 
                                      (32 (aload 5)) 
                                      (34 (invokevirtual (methodCP "add" "java.util.EnumSet" ((class "java.lang.Object")) boolean))) 
                                      (37 (pop)) 
                                      (38 (iinc 4 1)) 
                                      (41 (goto 19)) ;;to TAG_1
                                      (44 (aload_1)) ;;at TAG_0
                                      (45 (areturn)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *EnumSet$SerializationProxy-class-table*
  (make-static-class-decls 
   *java.util.EnumSet$SerializationProxy*))

(defconst *package-name-map* 
  ("java.util.EnumSet$SerializationProxy" . "java.util"))

