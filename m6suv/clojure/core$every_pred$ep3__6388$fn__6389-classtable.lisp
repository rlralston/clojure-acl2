; core$every_pred$ep3__6388$fn__6389-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$every_pred$ep3__6388$fn__6389*
 (make-class-def
      '(class "clojure.core$every_pred$ep3__6388$fn__6389"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "p2" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "p3" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "p1" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 80)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_1)) 
                                      (8 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (astore_2)) 
                                      (14 (aload_2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 75)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 76)) ;;to TAG_1
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload_1)) 
                                      (33 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (38 (astore_3)) 
                                      (39 (aload_3)) 
                                      (40 (dup)) 
                                      (41 (ifnull 68))  ;;to TAG_2
                                      (44 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (47 (if_acmpeq 69)) ;;to TAG_3
                                      (50 (aload_0)) 
                                      (51 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388$fn__6389" (class "java.lang.Object")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (aload_1)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_1)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (65 (goto 72)) ;;to TAG_4
                                      (68 (pop)) ;;at TAG_2
                                      (69 (aload_3)) ;;at TAG_3
                                      (70 (aconst_null)) 
                                      (71 (astore_3)) 
                                      (72 (goto 79)) ;;to TAG_5;;at TAG_4
                                      (75 (pop)) ;;at TAG_0
                                      (76 (aload_2)) ;;at TAG_1
                                      (77 (aconst_null)) 
                                      (78 (astore_2)) 
                                      (79 (areturn)) ;;at TAG_5
                                      (endofcode 80))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$every_pred$ep3__6388$fn__6389-class-table*
  (make-static-class-decls 
   *clojure.core$every_pred$ep3__6388$fn__6389*))

(defconst *package-name-map* 
  ("clojure.core$every_pred$ep3__6388$fn__6389" . "clojure"))

