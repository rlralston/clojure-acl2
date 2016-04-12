; Agent$Action-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Agent$Action*
 (make-class-def
      '(class "clojure.lang.Agent$Action"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "agent" (class "clojure.lang.Agent") (accessflags  *class*  *final* ) -1)
                        (field "fn" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "args" (class "clojure.lang.ISeq") (accessflags  *class*  *final* ) -1)
                        (field "exec" (class "java.util.concurrent.Executor") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.Agent") (class "clojure.lang.IFn") (class "clojure.lang.ISeq") (class "java.util.concurrent.Executor"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent"))))
                                      (9 (aload_0))
                                      (10 (aload_3))
                                      (11 (putfield (fieldCP "args" "clojure.lang.Agent$Action" (class "clojure.lang.ISeq"))))
                                      (14 (aload_0))
                                      (15 (aload_2))
                                      (16 (putfield (fieldCP "fn" "clojure.lang.Agent$Action" (class "clojure.lang.IFn"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "exec" "clojure.lang.Agent$Action" (class "java.util.concurrent.Executor"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "execute"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "exec" "clojure.lang.Agent$Action" (class "java.util.concurrent.Executor")))) 
                                      (4 (aload_0)) 
                                      (5 (invokeinterface (methodCP "execute" "java.util.concurrent.Executor" ((class "java.lang.Runnable")) void) 2)) 
                                      (10 (goto 46)) ;;to TAG_0;;at TAG_2
                                      (13 (astore_1)) ;;at TAG_3
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (18 (getfield (fieldCP "errorHandler" "clojure.lang.Agent" (class "clojure.lang.IFn")))) 
                                      (21 (ifnull 46)) ;;to TAG_0
                                      (24 (aload_0)) ;;at TAG_4
                                      (25 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (28 (getfield (fieldCP "errorHandler" "clojure.lang.Agent" (class "clojure.lang.IFn")))) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (35 (aload_1)) 
                                      (36 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (41 (pop)) 
                                      (42 (goto 46)) ;;to TAG_0;;at TAG_5
                                      (45 (astore_2)) ;;at TAG_6
                                      (46 (return)) ;;at TAG_0
                                      (endofcode 47))
                                   (Exceptions 
                                     (handler 0 10  13 (class "java.lang.Throwable"))
                                     (handler 24 42  45 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "doRun"
                              (parameters (class "clojure.lang.Agent$Action"))
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 248)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "nested" "clojure.lang.Agent" (class "java.lang.ThreadLocal")))) ;;at TAG_15
                                      (3 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (6 (invokevirtual (methodCP "set" "java.lang.ThreadLocal" ((class "java.lang.Object")) void))) 
                                      (9 (aconst_null)) 
                                      (10 (astore_1)) 
                                      (11 (aload_0)) ;;at TAG_9
                                      (12 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (15 (getfield (fieldCP "state" "clojure.lang.Agent" (class "java.lang.Object")))) 
                                      (18 (astore_2)) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "fn" "clojure.lang.Agent$Action" (class "clojure.lang.IFn")))) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (27 (getfield (fieldCP "state" "clojure.lang.Agent" (class "java.lang.Object")))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "args" "clojure.lang.Agent$Action" (class "clojure.lang.ISeq")))) 
                                      (34 (invokestatic (methodCP "cons" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (37 (invokeinterface (methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2)) 
                                      (42 (astore_3)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (47 (aload_3)) 
                                      (48 (invokevirtual (methodCP "setState" "clojure.lang.Agent" ((class "java.lang.Object")) boolean))) 
                                      (51 (pop)) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (56 (aload_2)) 
                                      (57 (aload_3)) 
                                      (58 (invokevirtual (methodCP "notifyWatches" "clojure.lang.Agent" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (61 (goto 67)) ;;to TAG_0;;at TAG_10
                                      (64 (astore_2)) ;;at TAG_11
                                      (65 (aload_2)) 
                                      (66 (astore_1)) 
                                      (67 (aload_1)) ;;at TAG_0
                                      (68 (ifnonnull 78)) ;;to TAG_1
                                      (71 (invokestatic (methodCP "releasePendingSends" "clojure.lang.Agent" () int))) 
                                      (74 (pop)) 
                                      (75 (goto 132)) ;;to TAG_2
                                      (78 (getstatic (fieldCP "nested" "clojure.lang.Agent" (class "java.lang.ThreadLocal")))) ;;at TAG_1
                                      (81 (aconst_null)) 
                                      (82 (invokevirtual (methodCP "set" "java.lang.ThreadLocal" ((class "java.lang.Object")) void))) 
                                      (85 (aload_0)) 
                                      (86 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (89 (getfield (fieldCP "errorHandler" "clojure.lang.Agent" (class "clojure.lang.IFn")))) 
                                      (92 (ifnull 117)) ;;to TAG_3
                                      (95 (aload_0)) ;;at TAG_12
                                      (96 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (99 (getfield (fieldCP "errorHandler" "clojure.lang.Agent" (class "clojure.lang.IFn")))) 
                                      (102 (aload_0)) 
                                      (103 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (106 (aload_1)) 
                                      (107 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (112 (pop)) 
                                      (113 (goto 117)) ;;to TAG_3;;at TAG_13
                                      (116 (astore_2)) ;;at TAG_14
                                      (117 (aload_0)) ;;at TAG_3
                                      (118 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (121 (getfield (fieldCP "errorMode" "clojure.lang.Agent" (class "clojure.lang.Keyword")))) 
                                      (124 (getstatic (fieldCP "CONTINUE" "clojure.lang.Agent" (class "clojure.lang.Keyword")))) 
                                      (127 (if_acmpne 132)) ;;to TAG_2
                                      (130 (aconst_null)) 
                                      (131 (astore_1)) 
                                      (132 (iconst_0)) ;;at TAG_2
                                      (133 (istore_2)) 
                                      (134 (aconst_null)) 
                                      (135 (astore_3)) 
                                      (136 (iload_2)) ;;at TAG_5
                                      (137 (ifne 191)) ;;to TAG_4
                                      (140 (aload_0)) 
                                      (141 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (144 (getfield (fieldCP "aq" "clojure.lang.Agent" (class "java.util.concurrent.atomic.AtomicReference")))) 
                                      (147 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicReference" () (class "java.lang.Object")))) 
                                      (150 (checkcast (class "clojure.lang.Agent$ActionQueue"))) 
                                      (153 (astore 4)) 
                                      (155 (new (class "clojure.lang.Agent$ActionQueue"))) 
                                      (158 (dup)) 
                                      (159 (aload 4)) 
                                      (161 (getfield (fieldCP "q" "clojure.lang.Agent$ActionQueue" (class "clojure.lang.IPersistentStack")))) 
                                      (164 (invokeinterface (methodCP "pop" "clojure.lang.IPersistentStack" () (class "clojure.lang.IPersistentStack")) 1)) 
                                      (169 (aload_1)) 
                                      (170 (invokespecial (methodCP "<init>" "clojure.lang.Agent$ActionQueue" ((class "clojure.lang.IPersistentStack") (class "java.lang.Throwable")) void))) 
                                      (173 (astore_3)) 
                                      (174 (aload_0)) 
                                      (175 (getfield (fieldCP "agent" "clojure.lang.Agent$Action" (class "clojure.lang.Agent")))) 
                                      (178 (getfield (fieldCP "aq" "clojure.lang.Agent" (class "java.util.concurrent.atomic.AtomicReference")))) 
                                      (181 (aload 4)) 
                                      (183 (aload_3)) 
                                      (184 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicReference" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (187 (istore_2)) 
                                      (188 (goto 136)) ;;to TAG_5
                                      (191 (aload_1)) ;;at TAG_4
                                      (192 (ifnonnull 222)) ;;to TAG_6
                                      (195 (aload_3)) 
                                      (196 (getfield (fieldCP "q" "clojure.lang.Agent$ActionQueue" (class "clojure.lang.IPersistentStack")))) 
                                      (199 (invokeinterface (methodCP "count" "clojure.lang.IPersistentStack" () int) 1)) 
                                      (204 (ifle 222)) ;;to TAG_6
                                      (207 (aload_3)) 
                                      (208 (getfield (fieldCP "q" "clojure.lang.Agent$ActionQueue" (class "clojure.lang.IPersistentStack")))) 
                                      (211 (invokeinterface (methodCP "peek" "clojure.lang.IPersistentStack" () (class "java.lang.Object")) 1)) 
                                      (216 (checkcast (class "clojure.lang.Agent$Action"))) 
                                      (219 (invokevirtual (methodCP "execute" "clojure.lang.Agent$Action" () void))) 
                                      (222 (jsr 236)) ;;to TAG_7;;at TAG_6
                                      (225 (goto 247)) ;;to TAG_8;;at TAG_16
                                      (228 (astore 5)) ;;at TAG_17
                                      (230 (jsr 236)) ;;to TAG_7
                                      (233 (aload 5)) ;;at TAG_18
                                      (235 (athrow)) 
                                      (236 (astore 6)) ;;at TAG_7
                                      (238 (getstatic (fieldCP "nested" "clojure.lang.Agent" (class "java.lang.ThreadLocal")))) 
                                      (241 (aconst_null)) 
                                      (242 (invokevirtual (methodCP "set" "java.lang.ThreadLocal" ((class "java.lang.Object")) void))) 
                                      (245 (ret 6)) 
                                      (247 (return)) ;;at TAG_8
                                      (endofcode 248))
                                   (Exceptions 
                                     (handler 11 61  64 (class "java.lang.Throwable"))
                                     (handler 95 113  116 (class "java.lang.Throwable"))
                                     (handler 0 225  228 (class "java.lang.Throwable"))
                                     (handler 228 233  228 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "doRun" "clojure.lang.Agent$Action" ((class "clojure.lang.Agent$Action")) void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Runnable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Agent$Action-class-table*
  (make-static-class-decls 
   *clojure.lang.Agent$Action*))

(defconst *package-name-map* 
  ("clojure.lang.Agent$Action" . "clojure.lang"))
