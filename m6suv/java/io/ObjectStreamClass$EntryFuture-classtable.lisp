; ObjectStreamClass$EntryFuture-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectStreamClass$EntryFuture*
 (make-class-def
      '(class "java.io.ObjectStreamClass$EntryFuture"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "unset" (class "java.lang.Object") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "owner" (class "java.lang.Thread") (accessflags  *class*  *final*  *private* ) -1)
                        (field "entry" (class "java.lang.Object") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (invokestatic
					(methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread"))))
                                      (8 (putfield (fieldCP "owner" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Thread"))))
                                      (11 (aload_0))
                                      (12 (getstatic (fieldCP "unset" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object"))))
                                      (15 (putfield (fieldCP "entry" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object"))))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "entry" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (4 (getstatic (fieldCP "unset" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (7 (if_acmpeq 12))  ;;to TAG_0
                                      (10 (iconst_0)) 
                                      (11 (ireturn)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (aload_1)) 
                                      (14 (putfield (fieldCP "entry" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (17 (aload_0)) 
                                      (18 (invokevirtual (methodCP "notifyAll" "java.lang.Object" () void))) 
                                      (21 (iconst_1)) 
                                      (22 (ireturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) ;;at TAG_1
                                      (3 (getfield (fieldCP "entry" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (6 (getstatic (fieldCP "unset" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (9 (if_acmpne 25)) ;;to TAG_0
                                      (12 (aload_0)) ;;at TAG_3
                                      (13 (invokevirtual (methodCP "wait" "java.lang.Object" () void))) 
                                      (16 (goto 2)) ;;to TAG_1;;at TAG_4
                                      (19 (astore_2)) ;;at TAG_5
                                      (20 (iconst_1)) 
                                      (21 (istore_1)) 
                                      (22 (goto 2)) ;;to TAG_1
                                      (25 (iload_1)) ;;at TAG_0
                                      (26 (ifeq 41))  ;;to TAG_2
                                      (29 (new (class "java.io.ObjectStreamClass$EntryFuture$1"))) 
                                      (32 (dup)) 
                                      (33 (aload_0)) 
                                      (34 (invokespecial (methodCP "<init>" "java.io.ObjectStreamClass$EntryFuture$1" ((class "java.io.ObjectStreamClass$EntryFuture")) void))) 
                                      (37 (invokestatic (methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction")) (class "java.lang.Object")))) 
                                      (40 (pop)) 
                                      (41 (aload_0)) ;;at TAG_2
                                      (42 (getfield (fieldCP "entry" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object")))) 
                                      (45 (areturn)) 
                                      (endofcode 46))
                                   (Exceptions 
                                     (handler 12 16  19 (class "java.lang.InterruptedException")))
                                   (StackMap )))
                        (method "getOwner"
                              (parameters )
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "owner" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Thread"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.ObjectStreamClass$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.ObjectStreamClass$EntryFuture" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.Object")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (7 (putstatic (fieldCP "unset" "java.io.ObjectStreamClass$EntryFuture" (class "java.lang.Object"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectStreamClass$EntryFuture-class-table*
  (make-static-class-decls 
   *java.io.ObjectStreamClass$EntryFuture*))

(defconst *package-name-map* 
  ("java.io.ObjectStreamClass$EntryFuture" . "java.io"))
