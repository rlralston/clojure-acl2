; Class$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.Class$3*
 (make-class-def
      '(class "java.lang.Class$3"
            "java.lang.Object"
            (constant_pool
                        (STRING  "sun.reflect.noCaches")
                        (STRING  "true"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 39)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "out" "java.lang.System" (class "java.io.PrintStream")))) 
                                      (3 (ifnonnull 8))  ;;to TAG_0
                                      (6 (aconst_null)) 
                                      (7 (areturn)) 
                                      (8 (ldc 0)) ;;at TAG_0;;STRING:: "sun.reflect.noCaches"
                                      (10 (invokestatic (methodCP "getProperty" "java.lang.System" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (13 (astore_1)) 
                                      (14 (aload_1)) 
                                      (15 (ifnull 32)) ;;to TAG_1
                                      (18 (aload_1)) 
                                      (19 (ldc 1)) ;;STRING:: "true"
                                      (21 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (24 (ifeq 32)) ;;to TAG_1
                                      (27 (iconst_0)) 
                                      (28 (invokestatic (methodCP "access$202" "java.lang.Class" (boolean) boolean))) 
                                      (31 (pop)) 
                                      (32 (iconst_1)) ;;at TAG_1
                                      (33 (invokestatic (methodCP "access$302" "java.lang.Class" (boolean) boolean))) 
                                      (36 (pop)) 
                                      (37 (aconst_null)) 
                                      (38 (areturn)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.lang.Class$3" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Class$3-class-table*
  (make-static-class-decls 
   *java.lang.Class$3*))

(defconst *package-name-map* 
  ("java.lang.Class$3" . "java.lang"))

