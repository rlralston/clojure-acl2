; LoggingProxyImpl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.LoggingProxyImpl*
 (make-class-def
      '(class "java.util.logging.LoggingProxyImpl"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "sun.util.logging.LoggingProxy") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLogger"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "getLogger" "java.util.logging.Logger" ((class "java.lang.String")) (class "java.util.logging.Logger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLevel"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (invokevirtual
					(methodCP "getLevel" "java.util.logging.Logger" () (class "java.util.logging.Level"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setLevel"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (aload_2))
                                      (5 (checkcast (class "java.util.logging.Level")))
                                      (8 (invokevirtual
					(methodCP "setLevel" "java.util.logging.Logger" ((class "java.util.logging.Level")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isLoggable"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (aload_2))
                                      (5 (checkcast (class "java.util.logging.Level")))
                                      (8 (invokevirtual
					(methodCP "isLoggable" "java.util.logging.Logger" ((class "java.util.logging.Level")) boolean)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "log"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (aload_2))
                                      (5 (checkcast (class "java.util.logging.Level")))
                                      (8 (aload_3))
                                      (9 (invokevirtual
					(methodCP "log" "java.util.logging.Logger" ((class "java.util.logging.Level") (class "java.lang.String")) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "log"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (aload_2))
                                      (5 (checkcast (class "java.util.logging.Level")))
                                      (8 (aload_3))
                                      (9 (aload 4))
                                      (11 (invokevirtual
					(methodCP "log" "java.util.logging.Logger" ((class "java.util.logging.Level") (class "java.lang.String") (class "java.lang.Throwable")) void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "log"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class*  *public*  *transient* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Logger")))
                                      (4 (aload_2))
                                      (5 (checkcast (class "java.util.logging.Level")))
                                      (8 (aload_3))
                                      (9 (aload 4))
                                      (11 (invokevirtual
					(methodCP "log" "java.util.logging.Logger" ((class "java.util.logging.Level") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLoggerNames"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLoggingMXBean" "java.util.logging.LogManager" () (class "java.util.logging.LoggingMXBean"))))
                                      (3 (invokeinterface
					(methodCP "getLoggerNames" "java.util.logging.LoggingMXBean" () (class "java.util.List")) 1))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLoggerLevel"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLoggingMXBean" "java.util.logging.LogManager" () (class "java.util.logging.LoggingMXBean"))))
                                      (3 (aload_1))
                                      (4 (invokeinterface
					(methodCP "getLoggerLevel" "java.util.logging.LoggingMXBean" ((class "java.lang.String")) (class "java.lang.String")) 2))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setLoggerLevel"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLoggingMXBean" "java.util.logging.LogManager" () (class "java.util.logging.LoggingMXBean"))))
                                      (3 (aload_1))
                                      (4 (aload_2))
                                      (5 (invokeinterface
					(methodCP "setLoggerLevel" "java.util.logging.LoggingMXBean" ((class "java.lang.String") (class "java.lang.String")) void) 3))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getParentLoggerName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLoggingMXBean" "java.util.logging.LogManager" () (class "java.util.logging.LoggingMXBean"))))
                                      (3 (aload_1))
                                      (4 (invokeinterface
					(methodCP "getParentLoggerName" "java.util.logging.LoggingMXBean" ((class "java.lang.String")) (class "java.lang.String")) 2))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parseLevel"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "parse" "java.util.logging.Level" ((class "java.lang.String")) (class "java.util.logging.Level"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLevelName"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.logging.Level")))
                                      (4 (invokevirtual
					(methodCP "getName" "java.util.logging.Level" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getProperty"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager"))))
                                      (3 (aload_1))
                                      (4 (invokevirtual
					(methodCP "getProperty" "java.util.logging.LogManager" ((class "java.lang.String")) (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.logging.LoggingProxyImpl")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.logging.LoggingProxyImpl" () void)))
                                      (7 (putstatic (fieldCP "INSTANCE" "java.util.logging.LoggingProxyImpl" (class "sun.util.logging.LoggingProxy"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.util.logging.LoggingProxy")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LoggingProxyImpl-class-table*
  (make-static-class-decls 
   *java.util.logging.LoggingProxyImpl*))

(defconst *package-name-map* 
  ("java.util.logging.LoggingProxyImpl" . "java.util.logging"))

