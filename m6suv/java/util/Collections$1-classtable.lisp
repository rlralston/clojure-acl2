; Collections$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$1*
 (make-class-def
      '(class "java.util.Collections$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "hasNext" boolean (accessflags  *class*  *private* ) -1)
                        (field "val$e" (class "java.lang.Object") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$e" "java.util.Collections$1" (class "java.lang.Object"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_1))
                                      (11 (putfield (fieldCP "hasNext" "java.util.Collections$1" boolean)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hasNext" "java.util.Collections$1" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "hasNext" "java.util.Collections$1" boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (iconst_0)) 
                                      (9 (putfield (fieldCP "hasNext" "java.util.Collections$1" boolean))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "val$e" "java.util.Collections$1" (class "java.lang.Object")))) 
                                      (16 (areturn)) 
                                      (17 (new (class "java.util.NoSuchElementException"))) ;;at TAG_0
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (24 (athrow)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$1-class-table*
  (make-static-class-decls 
   *java.util.Collections$1*))

(defconst *package-name-map* 
  ("java.util.Collections$1" . "java.util"))

