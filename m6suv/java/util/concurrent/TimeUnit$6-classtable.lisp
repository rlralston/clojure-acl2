; TimeUnit$6-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.TimeUnit$6*
 (make-class-def
      '(class "java.util.concurrent.TimeUnit$6"
            "java.util.concurrent.TimeUnit"
            (constant_pool
                        (LONG 3600000000000)
                        (LONG 2562047)
                        (LONG 3600000000)
                        (LONG 2562047788)
                        (LONG 3600000)
                        (LONG 2562047788015)
                        (LONG 3600)
                        (LONG 2562047788015215)
                        (LONG 60)
                        (LONG 153722867280912930)
                        (LONG 24))
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (aconst_null))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.TimeUnit" ((class "java.lang.String") int (class "java.util.concurrent.TimeUnit$1")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toNanos"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 0))      ;; LONG:: "3600000000000"
                                      (4 (ldc2_w 1))      ;; LONG:: "2562047"
                                      (7 (invokestatic
					(methodCP "x" "java.util.concurrent.TimeUnit$6" (long long long) long)))
                                      (10 (lreturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toMicros"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 2))      ;; LONG:: "3600000000"
                                      (4 (ldc2_w 3))      ;; LONG:: "2562047788"
                                      (7 (invokestatic
					(methodCP "x" "java.util.concurrent.TimeUnit$6" (long long long) long)))
                                      (10 (lreturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toMillis"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 4))      ;; LONG:: "3600000"
                                      (4 (ldc2_w 5))      ;; LONG:: "2562047788015"
                                      (7 (invokestatic
					(methodCP "x" "java.util.concurrent.TimeUnit$6" (long long long) long)))
                                      (10 (lreturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toSeconds"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 6))      ;; LONG:: "3600"
                                      (4 (ldc2_w 7))      ;; LONG:: "2562047788015215"
                                      (7 (invokestatic
					(methodCP "x" "java.util.concurrent.TimeUnit$6" (long long long) long)))
                                      (10 (lreturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toMinutes"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 8))      ;; LONG:: "60"
                                      (4 (ldc2_w 9))      ;; LONG:: "153722867280912930"
                                      (7 (invokestatic
					(methodCP "x" "java.util.concurrent.TimeUnit$6" (long long long) long)))
                                      (10 (lreturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toHours"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 2)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (lreturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toDays"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (lload_1))
                                      (1 (ldc2_w 10))     ;; LONG:: "24"
                                      (4 (ldiv))
                                      (5 (lreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "convert"
                              (parameters long (class "java.util.concurrent.TimeUnit"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_3))
                                      (1 (lload_1))
                                      (2 (invokevirtual
					(methodCP "toHours" "java.util.concurrent.TimeUnit" (long) long)))
                                      (5 (lreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "excessNanos"
                              (parameters long long)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 5) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *TimeUnit$6-class-table*
  (make-static-class-decls 
   *java.util.concurrent.TimeUnit$6*))

(defconst *package-name-map* 
  ("java.util.concurrent.TimeUnit$6" . "java.util.concurrent"))
