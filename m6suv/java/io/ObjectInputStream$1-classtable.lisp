; ObjectInputStream$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectInputStream$1*
 (make-class-def
      '(class "java.io.ObjectInputStream$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "readUnshared")
                        (STRING  "readFields"))
            (fields
                        (field "val$subcl" (class "java.lang.Class") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$subcl" "java.io.ObjectInputStream$1" (class "java.lang.Class"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Boolean"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 56)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$subcl" "java.io.ObjectInputStream$1" (class "java.lang.Class")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) ;;at TAG_1
                                      (6 (ldc_w )) 
                                      (9 (if_acmpeq 52)) ;;to TAG_0
                                      (12 (aload_1)) ;;at TAG_2
                                      (13 (ldc 0)) ;;STRING:: "readUnshared"
                                      (15 (aconst_null)) 
                                      (16 (checkcast (array (class "java.lang.Class")))) 
                                      (19 (invokevirtual (methodCP "getDeclaredMethod" "java.lang.Class" ((class "java.lang.String") (array (class "java.lang.Class"))) (class "java.lang.reflect.Method")))) 
                                      (22 (pop)) 
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (areturn)) ;;at TAG_3
                                      (27 (astore_2)) ;;at TAG_4
                                      (28 (aload_1)) ;;at TAG_5
                                      (29 (ldc 1)) ;;STRING:: "readFields"
                                      (31 (aconst_null)) 
                                      (32 (checkcast (array (class "java.lang.Class")))) 
                                      (35 (invokevirtual (methodCP "getDeclaredMethod" "java.lang.Class" ((class "java.lang.String") (array (class "java.lang.Class"))) (class "java.lang.reflect.Method")))) 
                                      (38 (pop)) 
                                      (39 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (42 (areturn)) ;;at TAG_6
                                      (43 (astore_2)) ;;at TAG_7
                                      (44 (aload_1)) 
                                      (45 (invokevirtual (methodCP "getSuperclass" "java.lang.Class" () (class "java.lang.Class")))) 
                                      (48 (astore_1)) 
                                      (49 (goto 5)) ;;to TAG_1
                                      (52 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (55 (areturn)) 
                                      (endofcode 56))
                                   (Exceptions 
                                     (handler 12 26  27 (class "java.lang.NoSuchMethodException"))
                                     (handler 28 42  43 (class "java.lang.NoSuchMethodException")))
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
					(methodCP "run" "java.io.ObjectInputStream$1" () (class "java.lang.Boolean"))))
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


(defconst *ObjectInputStream$1-class-table*
  (make-static-class-decls 
   *java.io.ObjectInputStream$1*))

(defconst *package-name-map* 
  ("java.io.ObjectInputStream$1" . "java.io"))
