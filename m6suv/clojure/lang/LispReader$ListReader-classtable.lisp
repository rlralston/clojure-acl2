; LispReader$ListReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$ListReader*
 (make-class-def
      '(class "clojure.lang.LispReader$ListReader"
            "clojure.lang.AFn"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 8) (code_length . 123)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.PushbackReader"))) 
                                      (4 (astore_3)) 
                                      (5 (iconst_m1)) 
                                      (6 (istore 4)) 
                                      (8 (iconst_m1)) 
                                      (9 (istore 5)) 
                                      (11 (aload_3)) 
                                      (12 (instanceof (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (15 (ifeq 38)) ;;to TAG_0
                                      (18 (aload_3)) 
                                      (19 (checkcast (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (22 (invokevirtual (methodCP "getLineNumber" "clojure.lang.LineNumberingPushbackReader" () int))) 
                                      (25 (istore 4)) 
                                      (27 (aload_3)) 
                                      (28 (checkcast (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (31 (invokevirtual (methodCP "getColumnNumber" "clojure.lang.LineNumberingPushbackReader" () int))) 
                                      (34 (iconst_1)) 
                                      (35 (isub)) 
                                      (36 (istore 5)) 
                                      (38 (bipush 41)) ;;at TAG_0
                                      (40 (aload_3)) 
                                      (41 (iconst_1)) 
                                      (42 (invokestatic (methodCP "readDelimitedList" "clojure.lang.LispReader" (char (class "java.io.PushbackReader") boolean) (class "java.util.List")))) 
                                      (45 (astore 6)) 
                                      (47 (aload 6)) 
                                      (49 (invokeinterface (methodCP "isEmpty" "java.util.List" () boolean) 1)) 
                                      (54 (ifeq 61)) ;;to TAG_1
                                      (57 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentList" (class "clojure.lang.PersistentList$EmptyList")))) 
                                      (60 (areturn)) 
                                      (61 (aload 6)) ;;at TAG_1
                                      (63 (invokestatic (methodCP "create" "clojure.lang.PersistentList" ((class "java.util.List")) (class "clojure.lang.IPersistentList")))) 
                                      (66 (checkcast (class "clojure.lang.IObj"))) 
                                      (69 (astore 7)) 
                                      (71 (iload 4)) 
                                      (73 (iconst_m1)) 
                                      (74 (if_icmpeq 120))  ;;to TAG_2
                                      (77 (aload 7)) 
                                      (79 (iconst_4)) 
                                      (80 (anewarray (class "java.lang.Object"))) 
                                      (83 (dup)) 
                                      (84 (iconst_0)) 
                                      (85 (getstatic (fieldCP "LINE_KEY" "clojure.lang.RT" (class "clojure.lang.Keyword")))) 
                                      (88 (aastore)) 
                                      (89 (dup)) 
                                      (90 (iconst_1)) 
                                      (91 (iload 4)) 
                                      (93 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (96 (aastore)) 
                                      (97 (dup)) 
                                      (98 (iconst_2)) 
                                      (99 (getstatic (fieldCP "COLUMN_KEY" "clojure.lang.RT" (class "clojure.lang.Keyword")))) 
                                      (102 (aastore)) 
                                      (103 (dup)) 
                                      (104 (iconst_3)) 
                                      (105 (iload 5)) 
                                      (107 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (110 (aastore)) 
                                      (111 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (114 (invokeinterface (methodCP "withMeta" "clojure.lang.IObj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj")) 2)) 
                                      (119 (areturn)) 
                                      (120 (aload 7)) ;;at TAG_2
                                      (122 (areturn)) 
                                      (endofcode 123))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$ListReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$ListReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$ListReader" . "clojure.lang"))

