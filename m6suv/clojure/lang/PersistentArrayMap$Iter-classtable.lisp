; PersistentArrayMap$Iter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentArrayMap$Iter*
 (make-class-def
      '(class "clojure.lang.PersistentArrayMap$Iter"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "array" (array (class "java.lang.Object")) (accessflags  *class* ) -1)
                        (field "i" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (bipush -2))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.lang.PersistentArrayMap$Iter" ((array (class "java.lang.Object")) int) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")) int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "array" "clojure.lang.PersistentArrayMap$Iter" (array (class "java.lang.Object")))))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "array" "clojure.lang.PersistentArrayMap$Iter" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (iconst_2)) 
                                      (10 (isub)) 
                                      (11 (if_icmpge 18))  ;;to TAG_0
                                      (14 (iconst_1)) 
                                      (15 (goto 19)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (ireturn)) ;;at TAG_1
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dup))
                                      (2 (getfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int)))
                                      (5 (iconst_2))
                                      (6 (iadd))
                                      (7 (putfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int)))
                                      (10 (new (class "clojure.lang.MapEntry")))
                                      (13 (dup))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "array" "clojure.lang.PersistentArrayMap$Iter" (array (class "java.lang.Object")))))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int)))
                                      (22 (aaload))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "array" "clojure.lang.PersistentArrayMap$Iter" (array (class "java.lang.Object")))))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "i" "clojure.lang.PersistentArrayMap$Iter" int)))
                                      (31 (iconst_1))
                                      (32 (iadd))
                                      (33 (aaload))
                                      (34 (invokespecial
					(methodCP "<init>" "clojure.lang.MapEntry" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (37 (areturn))
                                      (endofcode 38))
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
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PersistentArrayMap$Iter-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentArrayMap$Iter*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentArrayMap$Iter" . "clojure.lang"))

