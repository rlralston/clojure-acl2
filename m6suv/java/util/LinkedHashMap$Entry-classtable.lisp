; LinkedHashMap$Entry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.LinkedHashMap$Entry*
 (make-class-def
      '(class "java.util.LinkedHashMap$Entry"
            "java.util.HashMap$Entry"
            (constant_pool)
            (fields
                        (field "before" (class "java.util.LinkedHashMap$Entry") (accessflags  *class* ) -1)
                        (field "after" (class "java.util.LinkedHashMap$Entry") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (class "java.lang.Object") (class "java.lang.Object") (class "java.util.HashMap$Entry"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (aload 4))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.HashMap$Entry" (int (class "java.lang.Object") (class "java.lang.Object") (class "java.util.HashMap$Entry")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (8 (putfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (11 (aload_0))
                                      (12 (getfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (19 (putfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addBefore"
                              (parameters (class "java.util.LinkedHashMap$Entry"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (getfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (10 (putfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (17 (aload_0))
                                      (18 (putfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (25 (aload_0))
                                      (26 (putfield (fieldCP "before" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (29 (return))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "recordAccess"
                              (parameters (class "java.util.HashMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.util.LinkedHashMap"))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (invokestatic (methodCP "access$000" "java.util.LinkedHashMap" ((class "java.util.LinkedHashMap")) boolean))) 
                                      (9 (ifeq 34))  ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (dup)) 
                                      (14 (getfield (fieldCP "modCount" "java.util.LinkedHashMap" int))) 
                                      (17 (iconst_1)) 
                                      (18 (iadd)) 
                                      (19 (putfield (fieldCP "modCount" "java.util.LinkedHashMap" int))) 
                                      (22 (aload_0)) 
                                      (23 (invokespecial (methodCP "remove" "java.util.LinkedHashMap$Entry" () void))) 
                                      (26 (aload_0)) 
                                      (27 (aload_2)) 
                                      (28 (invokestatic (methodCP "access$100" "java.util.LinkedHashMap" ((class "java.util.LinkedHashMap")) (class "java.util.LinkedHashMap$Entry")))) 
                                      (31 (invokespecial (methodCP "addBefore" "java.util.LinkedHashMap$Entry" ((class "java.util.LinkedHashMap$Entry")) void))) 
                                      (34 (return)) ;;at TAG_0
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "recordRemoval"
                              (parameters (class "java.util.HashMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "remove" "java.util.LinkedHashMap$Entry" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$600"
                              (parameters (class "java.util.LinkedHashMap$Entry") (class "java.util.LinkedHashMap$Entry"))
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "addBefore" "java.util.LinkedHashMap$Entry" ((class "java.util.LinkedHashMap$Entry")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *LinkedHashMap$Entry-class-table*
  (make-static-class-decls 
   *java.util.LinkedHashMap$Entry*))

(defconst *package-name-map* 
  ("java.util.LinkedHashMap$Entry" . "java.util"))
