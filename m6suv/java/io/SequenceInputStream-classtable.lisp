; SequenceInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.SequenceInputStream*
 (make-class-def
      '(class "java.io.SequenceInputStream"
            "java.io.InputStream"
            (constant_pool
                        (STRING  "panic"))
            (fields
                        (field "e" (class "java.util.Enumeration") (accessflags  *class* ) -1)
                        (field "in" (class "java.io.InputStream") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Enumeration"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.io.InputStream" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (putfield (fieldCP "e" "java.io.SequenceInputStream" (class "java.util.Enumeration")))) 
                                      (9 (aload_0)) ;;at TAG_1
                                      (10 (invokevirtual (methodCP "nextStream" "java.io.SequenceInputStream" () void))) 
                                      (13 (goto 27)) ;;to TAG_0;;at TAG_2
                                      (16 (astore_2)) ;;at TAG_3
                                      (17 (new (class "java.lang.Error"))) 
                                      (20 (dup)) 
                                      (21 (ldc 0)) ;;STRING:: "panic"
                                      (23 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.String")) void))) 
                                      (26 (athrow)) 
                                      (27 (return)) ;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions 
                                     (handler 9 13  16 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream") (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.io.InputStream" () void))) 
                                      (4 (new (class "java.util.Vector"))) 
                                      (7 (dup)) 
                                      (8 (iconst_2)) 
                                      (9 (invokespecial (methodCP "<init>" "java.util.Vector" (int) void))) 
                                      (12 (astore_3)) 
                                      (13 (aload_3)) 
                                      (14 (aload_1)) 
                                      (15 (invokevirtual (methodCP "addElement" "java.util.Vector" ((class "java.lang.Object")) void))) 
                                      (18 (aload_3)) 
                                      (19 (aload_2)) 
                                      (20 (invokevirtual (methodCP "addElement" "java.util.Vector" ((class "java.lang.Object")) void))) 
                                      (23 (aload_0)) 
                                      (24 (aload_3)) 
                                      (25 (invokevirtual (methodCP "elements" "java.util.Vector" () (class "java.util.Enumeration")))) 
                                      (28 (putfield (fieldCP "e" "java.io.SequenceInputStream" (class "java.util.Enumeration")))) 
                                      (31 (aload_0)) ;;at TAG_1
                                      (32 (invokevirtual (methodCP "nextStream" "java.io.SequenceInputStream" () void))) 
                                      (35 (goto 50)) ;;to TAG_0;;at TAG_2
                                      (38 (astore 4)) ;;at TAG_3
                                      (40 (new (class "java.lang.Error"))) 
                                      (43 (dup)) 
                                      (44 (ldc 0)) ;;STRING:: "panic"
                                      (46 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.String")) void))) 
                                      (49 (athrow)) 
                                      (50 (return)) ;;at TAG_0
                                      (endofcode 51))
                                   (Exceptions 
                                     (handler 31 35  38 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "nextStream"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 63)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (4 (ifnull 14)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (11 (invokevirtual (methodCP "close" "java.io.InputStream" () void))) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (getfield (fieldCP "e" "java.io.SequenceInputStream" (class "java.util.Enumeration")))) 
                                      (18 (invokeinterface (methodCP "hasMoreElements" "java.util.Enumeration" () boolean) 1)) 
                                      (23 (ifeq 57)) ;;to TAG_1
                                      (26 (aload_0)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "e" "java.io.SequenceInputStream" (class "java.util.Enumeration")))) 
                                      (31 (invokeinterface (methodCP "nextElement" "java.util.Enumeration" () (class "java.lang.Object")) 1)) 
                                      (36 (checkcast (class "java.io.InputStream"))) 
                                      (39 (putfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (46 (ifnonnull 62))  ;;to TAG_2
                                      (49 (new (class "java.lang.NullPointerException"))) 
                                      (52 (dup)) 
                                      (53 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (56 (athrow)) 
                                      (57 (aload_0)) ;;at TAG_1
                                      (58 (aconst_null)) 
                                      (59 (putfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (62 (return)) ;;at TAG_2
                                      (endofcode 63))
                                   (Exceptions )
                                   (StackMap )))
                        (method "available"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (4 (ifnonnull 9))  ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (13 (invokevirtual (methodCP "available" "java.io.InputStream" () int))) 
                                      (16 (ireturn)) 
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (4 (ifnonnull 9))  ;;to TAG_0
                                      (7 (iconst_m1)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (13 (invokevirtual (methodCP "read" "java.io.InputStream" () int))) 
                                      (16 (istore_1)) 
                                      (17 (iload_1)) 
                                      (18 (iconst_m1)) 
                                      (19 (if_icmpne 31)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (invokevirtual (methodCP "nextStream" "java.io.SequenceInputStream" () void))) 
                                      (26 (aload_0)) 
                                      (27 (invokevirtual (methodCP "read" "java.io.SequenceInputStream" () int))) 
                                      (30 (ireturn)) 
                                      (31 (iload_1)) ;;at TAG_1
                                      (32 (ireturn)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 83)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (4 (ifnonnull 9)) ;;to TAG_0
                                      (7 (iconst_m1)) 
                                      (8 (ireturn)) 
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (ifnonnull 21)) ;;to TAG_1
                                      (13 (new (class "java.lang.NullPointerException"))) 
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (20 (athrow)) 
                                      (21 (iload_2)) ;;at TAG_1
                                      (22 (iflt 37))  ;;to TAG_2
                                      (25 (iload_3)) 
                                      (26 (iflt 37))  ;;to TAG_2
                                      (29 (iload_3)) 
                                      (30 (aload_1)) 
                                      (31 (arraylength)) 
                                      (32 (iload_2)) 
                                      (33 (isub)) 
                                      (34 (if_icmple 45)) ;;to TAG_3
                                      (37 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_2
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (44 (athrow)) 
                                      (45 (iload_3)) ;;at TAG_3
                                      (46 (ifne 51)) ;;to TAG_4
                                      (49 (iconst_0)) 
                                      (50 (ireturn)) 
                                      (51 (aload_0)) ;;at TAG_4
                                      (52 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (55 (aload_1)) 
                                      (56 (iload_2)) 
                                      (57 (iload_3)) 
                                      (58 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte) int int) int))) 
                                      (61 (istore 4)) 
                                      (63 (iload 4)) 
                                      (65 (ifgt 80)) ;;to TAG_5
                                      (68 (aload_0)) 
                                      (69 (invokevirtual (methodCP "nextStream" "java.io.SequenceInputStream" () void))) 
                                      (72 (aload_0)) 
                                      (73 (aload_1)) 
                                      (74 (iload_2)) 
                                      (75 (iload_3)) 
                                      (76 (invokevirtual (methodCP "read" "java.io.SequenceInputStream" ((array byte) int int) int))) 
                                      (79 (ireturn)) 
                                      (80 (iload 4)) ;;at TAG_5
                                      (82 (ireturn)) 
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokevirtual (methodCP "nextStream" "java.io.SequenceInputStream" () void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "in" "java.io.SequenceInputStream" (class "java.io.InputStream")))) 
                                      (8 (ifnonnull 0))  ;;to TAG_0
                                      (11 (return)) 
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SequenceInputStream-class-table*
  (make-static-class-decls 
   *java.io.SequenceInputStream*))

(defconst *package-name-map* 
  ("java.io.SequenceInputStream" . "java.io"))
