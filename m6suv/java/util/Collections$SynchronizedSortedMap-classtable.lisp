; Collections$SynchronizedSortedMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$SynchronizedSortedMap*
 (make-class-def
      '(class "java.util.Collections$SynchronizedSortedMap"
            "java.util.Collections$SynchronizedMap"
            (constant_pool
                        (LONG -8798146769416483793))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "sm" (class "java.util.SortedMap") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.SortedMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.Collections$SynchronizedMap" ((class "java.util.Map")) void)))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (putfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.SortedMap") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.util.Collections$SynchronizedMap" ((class "java.util.Map") (class "java.lang.Object")) void)))
                                      (6 (aload_0))
                                      (7 (aload_1))
                                      (8 (putfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "comparator"
                              (parameters )
                              (returntype . (class "java.util.Comparator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_0
                                      (8 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (11 (invokeinterface (methodCP "comparator" "java.util.SortedMap" () (class "java.util.Comparator")) 1)) 
                                      (16 (aload_1)) 
                                      (17 (monitorexit)) 
                                      (18 (areturn)) ;;at TAG_1
                                      (19 (astore_2)) ;;at TAG_2
                                      (20 (aload_1)) 
                                      (21 (monitorexit)) 
                                      (22 (aload_2)) ;;at TAG_3
                                      (23 (athrow)) 
                                      (endofcode 24))
                                   (Exceptions 
                                     (handler 7 18  19 (class "java.lang.Throwable"))
                                     (handler 19 22  19 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "subMap"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 39)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_3)) 
                                      (6 (monitorenter)) 
                                      (7 (new (class "java.util.Collections$SynchronizedSortedMap"))) ;;at TAG_0
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (15 (aload_1)) 
                                      (16 (aload_2)) 
                                      (17 (invokeinterface (methodCP "subMap" "java.util.SortedMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.util.SortedMap")) 3)) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (26 (invokespecial (methodCP "<init>" "java.util.Collections$SynchronizedSortedMap" ((class "java.util.SortedMap") (class "java.lang.Object")) void))) 
                                      (29 (aload_3)) 
                                      (30 (monitorexit)) 
                                      (31 (areturn)) ;;at TAG_1
                                      (32 (astore 4)) ;;at TAG_2
                                      (34 (aload_3)) 
                                      (35 (monitorexit)) 
                                      (36 (aload 4)) ;;at TAG_3
                                      (38 (athrow)) 
                                      (endofcode 39))
                                   (Exceptions 
                                     (handler 7 31  32 (class "java.lang.Throwable"))
                                     (handler 32 36  32 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "headMap"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_2)) 
                                      (6 (monitorenter)) 
                                      (7 (new (class "java.util.Collections$SynchronizedSortedMap"))) ;;at TAG_0
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (15 (aload_1)) 
                                      (16 (invokeinterface (methodCP "headMap" "java.util.SortedMap" ((class "java.lang.Object")) (class "java.util.SortedMap")) 2)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (25 (invokespecial (methodCP "<init>" "java.util.Collections$SynchronizedSortedMap" ((class "java.util.SortedMap") (class "java.lang.Object")) void))) 
                                      (28 (aload_2)) 
                                      (29 (monitorexit)) 
                                      (30 (areturn)) ;;at TAG_1
                                      (31 (astore_3)) ;;at TAG_2
                                      (32 (aload_2)) 
                                      (33 (monitorexit)) 
                                      (34 (aload_3)) ;;at TAG_3
                                      (35 (athrow)) 
                                      (endofcode 36))
                                   (Exceptions 
                                     (handler 7 30  31 (class "java.lang.Throwable"))
                                     (handler 31 34  31 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "tailMap"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_2)) 
                                      (6 (monitorenter)) 
                                      (7 (new (class "java.util.Collections$SynchronizedSortedMap"))) ;;at TAG_0
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (15 (aload_1)) 
                                      (16 (invokeinterface (methodCP "tailMap" "java.util.SortedMap" ((class "java.lang.Object")) (class "java.util.SortedMap")) 2)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (25 (invokespecial (methodCP "<init>" "java.util.Collections$SynchronizedSortedMap" ((class "java.util.SortedMap") (class "java.lang.Object")) void))) 
                                      (28 (aload_2)) 
                                      (29 (monitorexit)) 
                                      (30 (areturn)) ;;at TAG_1
                                      (31 (astore_3)) ;;at TAG_2
                                      (32 (aload_2)) 
                                      (33 (monitorexit)) 
                                      (34 (aload_3)) ;;at TAG_3
                                      (35 (athrow)) 
                                      (endofcode 36))
                                   (Exceptions 
                                     (handler 7 30  31 (class "java.lang.Throwable"))
                                     (handler 31 34  31 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "firstKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_0
                                      (8 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (11 (invokeinterface (methodCP "firstKey" "java.util.SortedMap" () (class "java.lang.Object")) 1)) 
                                      (16 (aload_1)) 
                                      (17 (monitorexit)) 
                                      (18 (areturn)) ;;at TAG_1
                                      (19 (astore_2)) ;;at TAG_2
                                      (20 (aload_1)) 
                                      (21 (monitorexit)) 
                                      (22 (aload_2)) ;;at TAG_3
                                      (23 (athrow)) 
                                      (endofcode 24))
                                   (Exceptions 
                                     (handler 7 18  19 (class "java.lang.Throwable"))
                                     (handler 19 22  19 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "lastKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "mutex" "java.util.Collections$SynchronizedSortedMap" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_0
                                      (8 (getfield (fieldCP "sm" "java.util.Collections$SynchronizedSortedMap" (class "java.util.SortedMap")))) 
                                      (11 (invokeinterface (methodCP "lastKey" "java.util.SortedMap" () (class "java.lang.Object")) 1)) 
                                      (16 (aload_1)) 
                                      (17 (monitorexit)) 
                                      (18 (areturn)) ;;at TAG_1
                                      (19 (astore_2)) ;;at TAG_2
                                      (20 (aload_1)) 
                                      (21 (monitorexit)) 
                                      (22 (aload_2)) ;;at TAG_3
                                      (23 (athrow)) 
                                      (endofcode 24))
                                   (Exceptions 
                                     (handler 7 18  19 (class "java.lang.Throwable"))
                                     (handler 19 22  19 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "java.util.SortedMap")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$SynchronizedSortedMap-class-table*
  (make-static-class-decls 
   *java.util.Collections$SynchronizedSortedMap*))

(defconst *package-name-map* 
  ("java.util.Collections$SynchronizedSortedMap" . "java.util"))
