; core$hash_combine-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$hash_combine*
 (make-class-def
      '(class "clojure.core$hash_combine"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields)
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "java.lang.Number")))
                                      (6 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int)))
                                      (9 (aload_2))
                                      (10 (aconst_null))
                                      (11 (astore_2))
                                      (12 (invokestatic
					(methodCP "hash" "clojure.lang.Util" ((class "java.lang.Object")) int)))
                                      (15 (invokestatic
					(methodCP "hashCombine" "clojure.lang.Util" (int int) int)))
                                      (18 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (21 (areturn))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$hash_combine-class-table*
  (make-static-class-decls 
   *clojure.core$hash_combine*))

(defconst *package-name-map* 
  ("clojure.core$hash_combine" . "clojure"))
