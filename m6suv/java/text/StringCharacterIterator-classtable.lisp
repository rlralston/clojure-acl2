; StringCharacterIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.text.StringCharacterIterator*
 (make-class-def
      '(class "java.text.StringCharacterIterator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Invalid substring range")
                        (STRING  "Invalid position")
                        (STRING  "Invalid index")
                        (INT 65535))
            (fields
                        (field "text" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "begin" int (accessflags  *class*  *private* ) -1)
                        (field "end" int (accessflags  *class*  *private* ) -1)
                        (field "pos" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_0))
                                      (3 (invokespecial
					(methodCP "<init>" "java.text.StringCharacterIterator" ((class "java.lang.String") int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_0))
                                      (3 (aload_1))
                                      (4 (invokevirtual
					(methodCP "length" "java.lang.String" () int)))
                                      (7 (iload_2))
                                      (8 (invokespecial
					(methodCP "<init>" "java.text.StringCharacterIterator" ((class "java.lang.String") int int int) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 87)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 16)) ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (putfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (21 (iload_2)) 
                                      (22 (iflt 38)) ;;to TAG_1
                                      (25 (iload_2)) 
                                      (26 (iload_3)) 
                                      (27 (if_icmpgt 38)) ;;to TAG_1
                                      (30 (iload_3)) 
                                      (31 (aload_1)) 
                                      (32 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (35 (if_icmple 48))  ;;to TAG_2
                                      (38 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_1
                                      (41 (dup)) 
                                      (42 (ldc 0)) ;;STRING:: "Invalid substring range"
                                      (44 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (47 (athrow)) 
                                      (48 (iload 4)) ;;at TAG_2
                                      (50 (iload_2)) 
                                      (51 (if_icmplt 60)) ;;to TAG_3
                                      (54 (iload 4)) 
                                      (56 (iload_3)) 
                                      (57 (if_icmple 70)) ;;to TAG_4
                                      (60 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_3
                                      (63 (dup)) 
                                      (64 (ldc 1)) ;;STRING:: "Invalid position"
                                      (66 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (69 (athrow)) 
                                      (70 (aload_0)) ;;at TAG_4
                                      (71 (iload_2)) 
                                      (72 (putfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (75 (aload_0)) 
                                      (76 (iload_3)) 
                                      (77 (putfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (80 (aload_0)) 
                                      (81 (iload 4)) 
                                      (83 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (86 (return)) 
                                      (endofcode 87))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setText"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (aload_1)) 
                                      (14 (putfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (17 (aload_0)) 
                                      (18 (iconst_0)) 
                                      (19 (putfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (22 (aload_0)) 
                                      (23 (aload_1)) 
                                      (24 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (27 (putfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (30 (aload_0)) 
                                      (31 (iconst_0)) 
                                      (32 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (35 (return)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "first"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int)))
                                      (5 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int)))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "current" "java.text.StringCharacterIterator" () char)))
                                      (12 (ireturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "last"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (8 (if_icmpeq 24))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (16 (iconst_1)) 
                                      (17 (isub)) 
                                      (18 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (21 (goto 32)) ;;to TAG_1
                                      (24 (aload_0)) ;;at TAG_0
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (29 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (invokevirtual (methodCP "current" "java.text.StringCharacterIterator" () char))) 
                                      (36 (ireturn)) 
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setIndex"
                              (parameters int)
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (5 (if_icmplt 16))  ;;to TAG_0
                                      (8 (iload_1)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (13 (if_icmple 26)) ;;to TAG_1
                                      (16 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_0
                                      (19 (dup)) 
                                      (20 (ldc 2)) ;;STRING:: "Invalid index"
                                      (22 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (25 (athrow)) 
                                      (26 (aload_0)) ;;at TAG_1
                                      (27 (iload_1)) 
                                      (28 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (31 (aload_0)) 
                                      (32 (invokevirtual (methodCP "current" "java.text.StringCharacterIterator" () char))) 
                                      (35 (ireturn)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "current"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (8 (if_icmplt 34))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (19 (if_icmpge 34))  ;;to TAG_0
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (30 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (33 (ireturn)) 
                                      (34 (ldc 3)) ;;at TAG_0;;INT:: "65535"
                                      (36 (ireturn)) 
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (8 (iconst_1)) 
                                      (9 (isub)) 
                                      (10 (if_icmpge 35))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (dup)) 
                                      (15 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (18 (iconst_1)) 
                                      (19 (iadd)) 
                                      (20 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (31 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (34 (ireturn)) 
                                      (35 (aload_0)) ;;at TAG_0
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (40 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (43 (ldc 3)) ;;INT:: "65535"
                                      (45 (ireturn)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previous"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (8 (if_icmple 33))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (dup)) 
                                      (13 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (16 (iconst_1)) 
                                      (17 (isub)) 
                                      (18 (putfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (29 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (32 (ireturn)) 
                                      (33 (ldc 3)) ;;at TAG_0;;INT:: "65535"
                                      (35 (ireturn)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBeginIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEndIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 87)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.text.StringCharacterIterator"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.text.StringCharacterIterator"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_0)) 
                                      (22 (invokevirtual (methodCP "hashCode" "java.text.StringCharacterIterator" () int))) 
                                      (25 (aload_2)) 
                                      (26 (invokevirtual (methodCP "hashCode" "java.text.StringCharacterIterator" () int))) 
                                      (29 (if_icmpeq 34))  ;;to TAG_2
                                      (32 (iconst_0)) 
                                      (33 (ireturn)) 
                                      (34 (aload_0)) ;;at TAG_2
                                      (35 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (38 (aload_2)) 
                                      (39 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String")))) 
                                      (42 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (45 (ifne 50)) ;;to TAG_3
                                      (48 (iconst_0)) 
                                      (49 (ireturn)) 
                                      (50 (aload_0)) ;;at TAG_3
                                      (51 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (54 (aload_2)) 
                                      (55 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int))) 
                                      (58 (if_icmpne 83)) ;;to TAG_4
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (65 (aload_2)) 
                                      (66 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int))) 
                                      (69 (if_icmpne 83)) ;;to TAG_4
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (76 (aload_2)) 
                                      (77 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int))) 
                                      (80 (if_icmpeq 85)) ;;to TAG_5
                                      (83 (iconst_0)) ;;at TAG_4
                                      (84 (ireturn)) 
                                      (85 (iconst_1)) ;;at TAG_5
                                      (86 (ireturn)) 
                                      (endofcode 87))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "text" "java.text.StringCharacterIterator" (class "java.lang.String"))))
                                      (4 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "pos" "java.text.StringCharacterIterator" int)))
                                      (11 (ixor))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "begin" "java.text.StringCharacterIterator" int)))
                                      (16 (ixor))
                                      (17 (aload_0))
                                      (18 (getfield (fieldCP "end" "java.text.StringCharacterIterator" int)))
                                      (21 (ixor))
                                      (22 (ireturn))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (checkcast (class "java.text.StringCharacterIterator"))) 
                                      (7 (astore_1)) 
                                      (8 (aload_1)) 
                                      (9 (areturn)) ;;at TAG_1
                                      (10 (astore_1)) ;;at TAG_2
                                      (11 (new (class "java.lang.InternalError"))) 
                                      (14 (dup)) 
                                      (15 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (18 (athrow)) 
                                      (endofcode 19))
                                   (Exceptions 
                                     (handler 0 9  10 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap ))))
            (interfaces "java.text.CharacterIterator")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *StringCharacterIterator-class-table*
  (make-static-class-decls 
   *java.text.StringCharacterIterator*))

(defconst *package-name-map* 
  ("java.text.StringCharacterIterator" . "java.text"))

