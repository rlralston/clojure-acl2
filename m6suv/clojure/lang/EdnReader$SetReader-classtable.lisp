; EdnReader$SetReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.EdnReader$SetReader*
 (make-class-def
      '(class "clojure.lang.EdnReader$SetReader"
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.io.PushbackReader")))
                                      (4 (astore 4))
                                      (6 (bipush 125))
                                      (8 (aload 4))
                                      (10 (iconst_1))
                                      (11 (aload_3))
                                      (12 (invokestatic
					(methodCP "readDelimitedList" "clojure.lang.EdnReader" (char (class "java.io.PushbackReader") boolean (class "java.lang.Object")) (class "java.util.List"))))
                                      (15 (invokestatic
					(methodCP "createWithCheck" "clojure.lang.PersistentHashSet" ((class "java.util.List")) (class "clojure.lang.PersistentHashSet"))))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *EdnReader$SetReader-class-table*
  (make-static-class-decls 
   *clojure.lang.EdnReader$SetReader*))

(defconst *package-name-map* 
  ("clojure.lang.EdnReader$SetReader" . "clojure.lang"))

