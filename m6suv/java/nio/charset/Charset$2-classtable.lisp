; Charset$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.charset.Charset$2*
 (make-class-def
      '(class "java.nio.charset.Charset$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$charsetName" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$charsetName" "java.nio.charset.Charset$2" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.nio.charset.Charset"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 43)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "access$000" "java.nio.charset.Charset" () (class "java.util.Iterator")))) 
                                      (3 (astore_1)) 
                                      (4 (aload_1)) ;;at TAG_2
                                      (5 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (10 (ifeq 41)) ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (19 (checkcast (class "java.nio.charset.spi.CharsetProvider"))) 
                                      (22 (astore_2)) 
                                      (23 (aload_2)) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "val$charsetName" "java.nio.charset.Charset$2" (class "java.lang.String")))) 
                                      (28 (invokevirtual (methodCP "charsetForName" "java.nio.charset.spi.CharsetProvider" ((class "java.lang.String")) (class "java.nio.charset.Charset")))) 
                                      (31 (astore_3)) 
                                      (32 (aload_3)) 
                                      (33 (ifnull 38)) ;;to TAG_1
                                      (36 (aload_3)) 
                                      (37 (areturn)) 
                                      (38 (goto 4))  ;;to TAG_2;;at TAG_1
                                      (41 (aconst_null)) ;;at TAG_0
                                      (42 (areturn)) 
                                      (endofcode 43))
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
					(methodCP "run" "java.nio.charset.Charset$2" () (class "java.nio.charset.Charset"))))
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


(defconst *Charset$2-class-table*
  (make-static-class-decls 
   *java.nio.charset.Charset$2*))

(defconst *package-name-map* 
  ("java.nio.charset.Charset$2" . "java.nio.charset"))

