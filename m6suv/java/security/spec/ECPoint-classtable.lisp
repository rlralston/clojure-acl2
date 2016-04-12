; ECPoint-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.spec.ECPoint*
 (make-class-def
      '(class "java.security.spec.ECPoint"
            "java.lang.Object"
            (constant_pool
                        (STRING  "affine coordinate x or y is null"))
            (fields
                        (field "x" (class "java.math.BigInteger") (accessflags  *class*  *final*  *private* ) -1)
                        (field "y" (class "java.math.BigInteger") (accessflags  *class*  *final*  *private* ) -1)
                        (field "POINT_INFINITY" (class "java.security.spec.ECPoint") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger"))))
                                      (9 (aload_0))
                                      (10 (aconst_null))
                                      (11 (putfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.math.BigInteger") (class "java.math.BigInteger"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnull 12))  ;;to TAG_0
                                      (8 (aload_2)) 
                                      (9 (ifnonnull 22)) ;;to TAG_1
                                      (12 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (ldc 0)) ;;STRING:: "affine coordinate x or y is null"
                                      (18 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (21 (athrow)) 
                                      (22 (aload_0)) ;;at TAG_1
                                      (23 (aload_1)) 
                                      (24 (putfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (27 (aload_0)) 
                                      (28 (aload_2)) 
                                      (29 (putfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (32 (return)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAffineX"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAffineY"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 65)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_0)) ;;at TAG_0
                                      (8 (getstatic (fieldCP "POINT_INFINITY" "java.security.spec.ECPoint" (class "java.security.spec.ECPoint")))) 
                                      (11 (if_acmpne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (instanceof (class "java.security.spec.ECPoint"))) 
                                      (20 (ifeq 63))  ;;to TAG_2
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (27 (aload_1)) 
                                      (28 (checkcast (class "java.security.spec.ECPoint"))) 
                                      (31 (getfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (34 (invokevirtual (methodCP "equals" "java.math.BigInteger" ((class "java.lang.Object")) boolean))) 
                                      (37 (ifeq 61)) ;;to TAG_3
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (44 (aload_1)) 
                                      (45 (checkcast (class "java.security.spec.ECPoint"))) 
                                      (48 (getfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (51 (invokevirtual (methodCP "equals" "java.math.BigInteger" ((class "java.lang.Object")) boolean))) 
                                      (54 (ifeq 61)) ;;to TAG_3
                                      (57 (iconst_1)) 
                                      (58 (goto 62)) ;;to TAG_4
                                      (61 (iconst_0)) ;;at TAG_3
                                      (62 (ireturn)) ;;at TAG_4
                                      (63 (iconst_0)) ;;at TAG_2
                                      (64 (ireturn)) 
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "POINT_INFINITY" "java.security.spec.ECPoint" (class "java.security.spec.ECPoint")))) 
                                      (4 (if_acmpne 9))  ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "x" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (13 (invokevirtual (methodCP "hashCode" "java.math.BigInteger" () int))) 
                                      (16 (iconst_5)) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "y" "java.security.spec.ECPoint" (class "java.math.BigInteger")))) 
                                      (21 (invokevirtual (methodCP "hashCode" "java.math.BigInteger" () int))) 
                                      (24 (iadd)) 
                                      (25 (ishl)) 
                                      (26 (ireturn)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.security.spec.ECPoint")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.security.spec.ECPoint" () void)))
                                      (7 (putstatic (fieldCP "POINT_INFINITY" "java.security.spec.ECPoint" (class "java.security.spec.ECPoint"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ECPoint-class-table*
  (make-static-class-decls 
   *java.security.spec.ECPoint*))

(defconst *package-name-map* 
  ("java.security.spec.ECPoint" . "java.security.spec"))
