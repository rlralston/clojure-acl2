; TreeMap$Values-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.TreeMap$Values*
 (make-class-def
      '(class "java.util.TreeMap$Values"
            "java.util.AbstractCollection"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.TreeMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.TreeMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.AbstractCollection" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (new (class "java.util.TreeMap$ValueIterator")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (12 (invokevirtual
					(methodCP "getFirstEntry" "java.util.TreeMap" () (class "java.util.TreeMap$Entry"))))
                                      (15 (invokespecial
					(methodCP "<init>" "java.util.TreeMap$ValueIterator" ((class "java.util.TreeMap") (class "java.util.TreeMap$Entry")) void)))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (4 (invokevirtual
					(methodCP "size" "java.util.TreeMap" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "containsValue" "java.util.TreeMap" ((class "java.lang.Object")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap")))) 
                                      (4 (invokevirtual (methodCP "getFirstEntry" "java.util.TreeMap" () (class "java.util.TreeMap$Entry")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) ;;at TAG_2
                                      (9 (ifnull 41)) ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (invokevirtual (methodCP "getValue" "java.util.TreeMap$Entry" () (class "java.lang.Object")))) 
                                      (16 (aload_1)) 
                                      (17 (invokestatic (methodCP "valEquals" "java.util.TreeMap" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (20 (ifeq 33)) ;;to TAG_1
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap")))) 
                                      (27 (aload_2)) 
                                      (28 (invokestatic (methodCP "access$000" "java.util.TreeMap" ((class "java.util.TreeMap") (class "java.util.TreeMap$Entry")) void))) 
                                      (31 (iconst_1)) 
                                      (32 (ireturn)) 
                                      (33 (aload_2)) ;;at TAG_1
                                      (34 (invokestatic (methodCP "successor" "java.util.TreeMap" ((class "java.util.TreeMap$Entry")) (class "java.util.TreeMap$Entry")))) 
                                      (37 (astore_2)) 
                                      (38 (goto 8))  ;;to TAG_2
                                      (41 (iconst_0)) ;;at TAG_0
                                      (42 (ireturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.TreeMap$Values" (class "java.util.TreeMap"))))
                                      (4 (invokevirtual
					(methodCP "clear" "java.util.TreeMap" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *TreeMap$Values-class-table*
  (make-static-class-decls 
   *java.util.TreeMap$Values*))

(defconst *package-name-map* 
  ("java.util.TreeMap$Values" . "java.util"))
