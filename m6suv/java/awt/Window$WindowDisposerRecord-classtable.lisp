; Window$WindowDisposerRecord-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.Window$WindowDisposerRecord*
 (make-class-def
      '(class "java.awt.Window$WindowDisposerRecord"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "owner" (class "java.lang.ref.WeakReference") (accessflags  *class*  *final* ) -1)
                        (field "weakThis" (class "java.lang.ref.WeakReference") (accessflags  *class*  *final* ) -1)
                        (field "context" (class "java.lang.ref.WeakReference") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "sun.awt.AppContext") (class "java.awt.Window"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.lang.ref.WeakReference")))
                                      (8 (dup))
                                      (9 (aload_2))
                                      (10 (invokevirtual
					(methodCP "getOwner" "java.awt.Window" () (class "java.awt.Window"))))
                                      (13 (invokespecial
					(methodCP "<init>" "java.lang.ref.WeakReference" ((class "java.lang.Object")) void)))
                                      (16 (putfield (fieldCP "owner" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference"))))
                                      (19 (aload_0))
                                      (20 (aload_2))
                                      (21 (invokestatic
					(methodCP "access$000" "java.awt.Window" ((class "java.awt.Window")) (class "java.lang.ref.WeakReference"))))
                                      (24 (putfield (fieldCP "weakThis" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference"))))
                                      (27 (aload_0))
                                      (28 (new (class "java.lang.ref.WeakReference")))
                                      (31 (dup))
                                      (32 (aload_1))
                                      (33 (invokespecial
					(methodCP "<init>" "java.lang.ref.WeakReference" ((class "java.lang.Object")) void)))
                                      (36 (putfield (fieldCP "context" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference"))))
                                      (39 (return))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "dispose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "owner" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference")))) 
                                      (4 (invokevirtual (methodCP "get" "java.lang.ref.WeakReference" () (class "java.lang.Object")))) 
                                      (7 (checkcast (class "java.awt.Window"))) 
                                      (10 (astore_1)) 
                                      (11 (aload_1)) 
                                      (12 (ifnull 23))  ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "weakThis" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference")))) 
                                      (20 (invokevirtual (methodCP "removeOwnedWindow" "java.awt.Window" ((class "java.lang.ref.WeakReference")) void))) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "context" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference")))) 
                                      (27 (invokevirtual (methodCP "get" "java.lang.ref.WeakReference" () (class "java.lang.Object")))) 
                                      (30 (checkcast (class "sun.awt.AppContext"))) 
                                      (33 (astore_2)) 
                                      (34 (aconst_null)) 
                                      (35 (aload_2)) 
                                      (36 (if_acmpeq 47)) ;;to TAG_1
                                      (39 (aload_2)) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "weakThis" "java.awt.Window$WindowDisposerRecord" (class "java.lang.ref.WeakReference")))) 
                                      (44 (invokestatic (methodCP "access$100" "java.awt.Window" ((class "sun.awt.AppContext") (class "java.lang.ref.WeakReference")) void))) 
                                      (47 (return)) ;;at TAG_1
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.java2d.DisposerRecord")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Window$WindowDisposerRecord-class-table*
  (make-static-class-decls 
   *java.awt.Window$WindowDisposerRecord*))

(defconst *package-name-map* 
  ("java.awt.Window$WindowDisposerRecord" . "java.awt"))
