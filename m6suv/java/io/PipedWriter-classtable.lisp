; PipedWriter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.PipedWriter*
 (make-class-def
      '(class "java.io.PipedWriter"
            "java.io.Writer"
            (constant_pool
                        (STRING  "Already connected")
                        (STRING  "Pipe closed")
                        (STRING  "Pipe not connected"))
            (fields
                        (field "sink" (class "java.io.PipedReader") (accessflags  *class*  *private* ) -1)
                        (field "closed" boolean (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.PipedReader"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.Writer" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "closed" "java.io.PipedWriter" boolean)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (invokevirtual
					(methodCP "connect" "java.io.PipedWriter" ((class "java.io.PipedReader")) void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.Writer" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "closed" "java.io.PipedWriter" boolean)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "connect"
                              (parameters (class "java.io.PipedReader"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 81)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (16 (ifnonnull 26)) ;;to TAG_1
                                      (19 (aload_1)) 
                                      (20 (getfield (fieldCP "connected" "java.io.PipedReader" boolean))) 
                                      (23 (ifeq 36))  ;;to TAG_2
                                      (26 (new (class "java.io.IOException"))) ;;at TAG_1
                                      (29 (dup)) 
                                      (30 (ldc 0)) ;;STRING:: "Already connected"
                                      (32 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (35 (athrow)) 
                                      (36 (aload_1)) ;;at TAG_2
                                      (37 (getfield (fieldCP "closedByReader" "java.io.PipedReader" boolean))) 
                                      (40 (ifne 50)) ;;to TAG_3
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "closed" "java.io.PipedWriter" boolean))) 
                                      (47 (ifeq 60)) ;;to TAG_4
                                      (50 (new (class "java.io.IOException"))) ;;at TAG_3
                                      (53 (dup)) 
                                      (54 (ldc 1)) ;;STRING:: "Pipe closed"
                                      (56 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (59 (athrow)) 
                                      (60 (aload_0)) ;;at TAG_4
                                      (61 (aload_1)) 
                                      (62 (putfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (65 (aload_1)) 
                                      (66 (iconst_m1)) 
                                      (67 (putfield (fieldCP "in" "java.io.PipedReader" int))) 
                                      (70 (aload_1)) 
                                      (71 (iconst_0)) 
                                      (72 (putfield (fieldCP "out" "java.io.PipedReader" int))) 
                                      (75 (aload_1)) 
                                      (76 (iconst_1)) 
                                      (77 (putfield (fieldCP "connected" "java.io.PipedReader" boolean))) 
                                      (80 (return)) 
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (4 (ifnonnull 17))  ;;to TAG_0
                                      (7 (new (class "java.io.IOException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 2)) ;;STRING:: "Pipe not connected"
                                      (13 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (21 (iload_1)) 
                                      (22 (invokevirtual (methodCP "receive" "java.io.PipedReader" (int) void))) 
                                      (25 (return)) 
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters (array char) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 53)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (4 (ifnonnull 17))  ;;to TAG_0
                                      (7 (new (class "java.io.IOException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 2)) ;;STRING:: "Pipe not connected"
                                      (13 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (iload_2)) ;;at TAG_0
                                      (18 (iload_3)) 
                                      (19 (ior)) 
                                      (20 (iload_2)) 
                                      (21 (iload_3)) 
                                      (22 (iadd)) 
                                      (23 (ior)) 
                                      (24 (aload_1)) 
                                      (25 (arraylength)) 
                                      (26 (iload_2)) 
                                      (27 (iload_3)) 
                                      (28 (iadd)) 
                                      (29 (isub)) 
                                      (30 (ior)) 
                                      (31 (ifge 42)) ;;to TAG_1
                                      (34 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (37 (dup)) 
                                      (38 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (41 (athrow)) 
                                      (42 (aload_0)) ;;at TAG_1
                                      (43 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (46 (aload_1)) 
                                      (47 (iload_2)) 
                                      (48 (iload_3)) 
                                      (49 (invokevirtual (methodCP "receive" "java.io.PipedReader" ((array char) int int) void))) 
                                      (52 (return)) 
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap )))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (4 (ifnull 58)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (11 (getfield (fieldCP "closedByReader" "java.io.PipedReader" boolean))) 
                                      (14 (ifne 24)) ;;to TAG_1
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "closed" "java.io.PipedWriter" boolean))) 
                                      (21 (ifeq 34))  ;;to TAG_2
                                      (24 (new (class "java.io.IOException"))) ;;at TAG_1
                                      (27 (dup)) 
                                      (28 (ldc 1)) ;;STRING:: "Pipe closed"
                                      (30 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (33 (athrow)) 
                                      (34 (aload_0)) ;;at TAG_2
                                      (35 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (38 (dup)) 
                                      (39 (astore_1)) 
                                      (40 (monitorenter)) 
                                      (41 (aload_0)) ;;at TAG_3
                                      (42 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (45 (invokevirtual (methodCP "notifyAll" "java.lang.Object" () void))) 
                                      (48 (aload_1)) 
                                      (49 (monitorexit)) 
                                      (50 (goto 58)) ;;to TAG_0;;at TAG_4
                                      (53 (astore_2)) ;;at TAG_5
                                      (54 (aload_1)) 
                                      (55 (monitorexit)) 
                                      (56 (aload_2)) ;;at TAG_6
                                      (57 (athrow)) 
                                      (58 (return)) ;;at TAG_0
                                      (endofcode 59))
                                   (Exceptions 
                                     (handler 41 50  53 (class "java.lang.Throwable"))
                                     (handler 53 56  53 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_1)) 
                                      (2 (putfield (fieldCP "closed" "java.io.PipedWriter" boolean))) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (9 (ifnull 19))  ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "sink" "java.io.PipedWriter" (class "java.io.PipedReader")))) 
                                      (16 (invokevirtual (methodCP "receivedLast" "java.io.PipedReader" () void))) 
                                      (19 (return)) ;;at TAG_0
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PipedWriter-class-table*
  (make-static-class-decls 
   *java.io.PipedWriter*))

(defconst *package-name-map* 
  ("java.io.PipedWriter" . "java.io"))

