; Logging-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.Logging*
 (make-class-def
      '(class "java.util.logging.Logging"
            "java.lang.Object"
            (constant_pool
                        (STRING  "loggerName is null")
                        (STRING  "Logger ")
                        (STRING  "does not exist")
                        (STRING  ""))
            (fields
                        (field "logManager" (class "java.util.logging.LogManager") (accessflags  *class*  *private*  *static* ) -1)
                        (field "EMPTY_STRING" (class "java.lang.String") (accessflags  *class*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
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
                        (method "getLoggerNames"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "logManager" "java.util.logging.Logging" (class "java.util.logging.LogManager")))) 
                                      (3 (invokevirtual (methodCP "getLoggerNames" "java.util.logging.LogManager" () (class "java.util.Enumeration")))) 
                                      (6 (astore_1)) 
                                      (7 (new (class "java.util.ArrayList"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.util.ArrayList" () void))) 
                                      (14 (astore_2)) 
                                      (15 (aload_1)) ;;at TAG_1
                                      (16 (invokeinterface (methodCP "hasMoreElements" "java.util.Enumeration" () boolean) 1)) 
                                      (21 (ifeq 41))  ;;to TAG_0
                                      (24 (aload_2)) 
                                      (25 (aload_1)) 
                                      (26 (invokeinterface (methodCP "nextElement" "java.util.Enumeration" () (class "java.lang.Object")) 1)) 
                                      (31 (checkcast (class "java.lang.String"))) 
                                      (34 (invokevirtual (methodCP "add" "java.util.ArrayList" ((class "java.lang.Object")) boolean))) 
                                      (37 (pop)) 
                                      (38 (goto 15)) ;;to TAG_1
                                      (41 (aload_2)) ;;at TAG_0
                                      (42 (areturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLoggerLevel"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 32)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "logManager" "java.util.logging.Logging" (class "java.util.logging.LogManager")))) 
                                      (3 (aload_1)) 
                                      (4 (invokevirtual (methodCP "getLogger" "java.util.logging.LogManager" ((class "java.lang.String")) (class "java.util.logging.Logger")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (ifnonnull 14))  ;;to TAG_0
                                      (12 (aconst_null)) 
                                      (13 (areturn)) 
                                      (14 (aload_2)) ;;at TAG_0
                                      (15 (invokevirtual (methodCP "getLevel" "java.util.logging.Logger" () (class "java.util.logging.Level")))) 
                                      (18 (astore_3)) 
                                      (19 (aload_3)) 
                                      (20 (ifnonnull 27)) ;;to TAG_1
                                      (23 (getstatic (fieldCP "EMPTY_STRING" "java.util.logging.Logging" (class "java.lang.String")))) 
                                      (26 (areturn)) 
                                      (27 (aload_3)) ;;at TAG_1
                                      (28 (invokevirtual (methodCP "getName" "java.util.logging.Level" () (class "java.lang.String")))) 
                                      (31 (areturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setLoggerLevel"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 78)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 14)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 0)) ;;STRING:: "loggerName is null"
                                      (10 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (getstatic (fieldCP "logManager" "java.util.logging.Logging" (class "java.util.logging.LogManager")))) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (invokevirtual (methodCP "getLogger" "java.util.logging.LogManager" ((class "java.lang.String")) (class "java.util.logging.Logger")))) 
                                      (21 (astore_3)) 
                                      (22 (aload_3)) 
                                      (23 (ifnonnull 58)) ;;to TAG_1
                                      (26 (new (class "java.lang.IllegalArgumentException"))) 
                                      (29 (dup)) 
                                      (30 (new (class "java.lang.StringBuilder"))) 
                                      (33 (dup)) 
                                      (34 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (37 (ldc 1)) ;;STRING:: "Logger "
                                      (39 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (42 (aload_1)) 
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (46 (ldc 2)) ;;STRING:: "does not exist"
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (54 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (57 (athrow)) 
                                      (58 (aconst_null)) ;;at TAG_1
                                      (59 (astore 4)) 
                                      (61 (aload_2)) 
                                      (62 (ifnull 71))  ;;to TAG_2
                                      (65 (aload_2)) 
                                      (66 (invokestatic (methodCP "parse" "java.util.logging.Level" ((class "java.lang.String")) (class "java.util.logging.Level")))) 
                                      (69 (astore 4)) 
                                      (71 (aload_3)) ;;at TAG_2
                                      (72 (aload 4)) 
                                      (74 (invokevirtual (methodCP "setLevel" "java.util.logging.Logger" ((class "java.util.logging.Level")) void))) 
                                      (77 (return)) 
                                      (endofcode 78))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getParentLoggerName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 32)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "logManager" "java.util.logging.Logging" (class "java.util.logging.LogManager")))) 
                                      (3 (aload_1)) 
                                      (4 (invokevirtual (methodCP "getLogger" "java.util.logging.LogManager" ((class "java.lang.String")) (class "java.util.logging.Logger")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (ifnonnull 14))  ;;to TAG_0
                                      (12 (aconst_null)) 
                                      (13 (areturn)) 
                                      (14 (aload_2)) ;;at TAG_0
                                      (15 (invokevirtual (methodCP "getParent" "java.util.logging.Logger" () (class "java.util.logging.Logger")))) 
                                      (18 (astore_3)) 
                                      (19 (aload_3)) 
                                      (20 (ifnonnull 27)) ;;to TAG_1
                                      (23 (getstatic (fieldCP "EMPTY_STRING" "java.util.logging.Logging" (class "java.lang.String")))) 
                                      (26 (areturn)) 
                                      (27 (aload_3)) ;;at TAG_1
                                      (28 (invokevirtual (methodCP "getName" "java.util.logging.Logger" () (class "java.lang.String")))) 
                                      (31 (areturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 12)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager"))))
                                      (3 (putstatic (fieldCP "logManager" "java.util.logging.Logging" (class "java.util.logging.LogManager"))))
                                      (6 (ldc 3))         ;;STRING:: ""
                                      (8 (putstatic (fieldCP "EMPTY_STRING" "java.util.logging.Logging" (class "java.lang.String"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.logging.LoggingMXBean")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Logging-class-table*
  (make-static-class-decls 
   *java.util.logging.Logging*))

(defconst *package-name-map* 
  ("java.util.logging.Logging" . "java.util.logging"))

