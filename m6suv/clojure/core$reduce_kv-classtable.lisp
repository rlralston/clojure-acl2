; core$reduce_kv-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$reduce_kv*
 (make-class-def
      '(class "clojure.core$reduce_kv"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core.protocols")
                        (STRING  "kv-reduce"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__cached_class__0" (class "java.lang.Class") (accessflags  *class*  *private* ) -1)
                        (field "__cached_proto_fn__0" (class "clojure.lang.AFunction") (accessflags  *class*  *private* ) -1)
                        (field "__cached_proto_impl__0" (class "clojure.lang.IFn") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core.protocols"
                                      (2 (ldc 1))         ;;STRING:: "kv-reduce"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$reduce_kv" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 63)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_3)) 
                                      (3 (dup)) 
                                      (4 (invokestatic (methodCP "classOf" "clojure.lang.Util" ((class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "__cached_class__0" "clojure.core$reduce_kv" (class "java.lang.Class")))) 
                                      (11 (if_acmpeq 30)) ;;to TAG_0
                                      (14 (dup)) 
                                      (15 (instanceof (class "clojure.core.protocols.IKVReduce"))) 
                                      (18 (ifne 51)) ;;to TAG_1
                                      (21 (dup)) 
                                      (22 (invokestatic (methodCP "classOf" "clojure.lang.Util" ((class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (25 (aload_0)) 
                                      (26 (swap)) 
                                      (27 (putfield (fieldCP "__cached_class__0" "clojure.core$reduce_kv" (class "java.lang.Class")))) 
                                      (30 (getstatic (fieldCP "const__0" "clojure.core$reduce_kv" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (swap)) 
                                      (37 (aload_1)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_1)) 
                                      (40 (aload_2)) 
                                      (41 (aconst_null)) 
                                      (42 (astore_2)) 
                                      (43 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (48 (goto 62))  ;;to TAG_2
                                      (51 (aload_1)) ;;at TAG_1
                                      (52 (aconst_null)) 
                                      (53 (astore_1)) 
                                      (54 (aload_2)) 
                                      (55 (aconst_null)) 
                                      (56 (astore_2)) 
                                      (57 (invokeinterface (methodCP "kv_reduce" "clojure.core.protocols.IKVReduce" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (62 (areturn)) ;;at TAG_2
                                      (endofcode 63))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$reduce_kv-class-table*
  (make-static-class-decls 
   *clojure.core$reduce_kv*))

(defconst *package-name-map* 
  ("clojure.core$reduce_kv" . "clojure"))

