; ActivationID-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.activation.ActivationID*
 (make-class-def
      '(class "java.rmi.activation.ActivationID"
            "java.lang.Object"
            (constant_pool
                        (LONG -4608673054848209235)
                        (STRING  "activation failed")
                        (STRING  "unexpected invocation handler")
                        (STRING  "unexpected activator type")
                        (STRING  "sun.rmi.server.")
                        (STRING  "Unable to create remote reference"))
            (fields
                        (field "activator" (class "java.rmi.activation.Activator") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "uid" (class "java.rmi.server.UID") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.rmi.activation.Activator"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.rmi.server.UID")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.rmi.server.UID" () void)))
                                      (12 (putfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID"))))
                                      (15 (aload_0))
                                      (16 (aload_1))
                                      (17 (putfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "activate"
                              (parameters boolean)
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (4 (aload_0)) 
                                      (5 (iload_1)) 
                                      (6 (invokeinterface (methodCP "activate" "java.rmi.activation.Activator" ((class "java.rmi.activation.ActivationID") boolean) (class "java.rmi.MarshalledObject")) 3)) 
                                      (11 (astore_2)) 
                                      (12 (aload_2)) 
                                      (13 (invokevirtual (methodCP "get" "java.rmi.MarshalledObject" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "java.rmi.Remote"))) 
                                      (19 (areturn)) ;;at TAG_1
                                      (20 (astore_2)) ;;at TAG_2
                                      (21 (aload_2)) 
                                      (22 (athrow)) 
                                      (23 (astore_2)) ;;at TAG_3
                                      (24 (new (class "java.rmi.UnmarshalException"))) 
                                      (27 (dup)) 
                                      (28 (ldc 1)) ;;STRING:: "activation failed"
                                      (30 (aload_2)) 
                                      (31 (invokespecial (methodCP "<init>" "java.rmi.UnmarshalException" ((class "java.lang.String") (class "java.lang.Exception")) void))) 
                                      (34 (athrow)) 
                                      (35 (astore_2)) ;;at TAG_4
                                      (36 (new (class "java.rmi.UnmarshalException"))) 
                                      (39 (dup)) 
                                      (40 (ldc 1)) ;;STRING:: "activation failed"
                                      (42 (aload_2)) 
                                      (43 (invokespecial (methodCP "<init>" "java.rmi.UnmarshalException" ((class "java.lang.String") (class "java.lang.Exception")) void))) 
                                      (46 (athrow)) 
                                      (endofcode 47))
                                   (Exceptions 
                                     (handler 0 19  20 (class "java.rmi.RemoteException"))
                                     (handler 0 19  23 (class "java.io.IOException"))
                                     (handler 0 19  35 (class "java.lang.ClassNotFoundException")))
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID"))))
                                      (4 (invokevirtual
					(methodCP "hashCode" "java.rmi.server.UID" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.rmi.activation.ActivationID"))) 
                                      (4 (ifeq 46)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.rmi.activation.ActivationID"))) 
                                      (11 (astore_2)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID")))) 
                                      (16 (aload_2)) 
                                      (17 (getfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID")))) 
                                      (20 (invokevirtual (methodCP "equals" "java.rmi.server.UID" ((class "java.lang.Object")) boolean))) 
                                      (23 (ifeq 44)) ;;to TAG_1
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (30 (aload_2)) 
                                      (31 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (34 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (37 (ifeq 44)) ;;to TAG_1
                                      (40 (iconst_1)) 
                                      (41 (goto 45))  ;;to TAG_2
                                      (44 (iconst_0)) ;;at TAG_1
                                      (45 (ireturn)) ;;at TAG_2
                                      (46 (iconst_0)) ;;at TAG_0
                                      (47 (ireturn)) 
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 110)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID")))) 
                                      (5 (invokevirtual (methodCP "writeObject" "java.io.ObjectOutputStream" ((class "java.lang.Object")) void))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (12 (instanceof (class "java.rmi.server.RemoteObject"))) 
                                      (15 (ifeq 32)) ;;to TAG_0
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (22 (checkcast (class "java.rmi.server.RemoteObject"))) 
                                      (25 (invokevirtual (methodCP "getRef" "java.rmi.server.RemoteObject" () (class "java.rmi.server.RemoteRef")))) 
                                      (28 (astore_2)) 
                                      (29 (goto 91)) ;;to TAG_1
                                      (32 (aload_0)) ;;at TAG_0
                                      (33 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (36 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (39 (invokestatic (methodCP "isProxyClass" "java.lang.reflect.Proxy" ((class "java.lang.Class")) boolean))) 
                                      (42 (ifeq 81))  ;;to TAG_2
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (49 (invokestatic (methodCP "getInvocationHandler" "java.lang.reflect.Proxy" ((class "java.lang.Object")) (class "java.lang.reflect.InvocationHandler")))) 
                                      (52 (astore_3)) 
                                      (53 (aload_3)) 
                                      (54 (instanceof (class "java.rmi.server.RemoteObjectInvocationHandler"))) 
                                      (57 (ifne 70)) ;;to TAG_3
                                      (60 (new (class "java.io.InvalidObjectException"))) 
                                      (63 (dup)) 
                                      (64 (ldc 2)) ;;STRING:: "unexpected invocation handler"
                                      (66 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (69 (athrow)) 
                                      (70 (aload_3)) ;;at TAG_3
                                      (71 (checkcast (class "java.rmi.server.RemoteObjectInvocationHandler"))) 
                                      (74 (invokevirtual (methodCP "getRef" "java.rmi.server.RemoteObjectInvocationHandler" () (class "java.rmi.server.RemoteRef")))) 
                                      (77 (astore_2)) 
                                      (78 (goto 91)) ;;to TAG_1
                                      (81 (new (class "java.io.InvalidObjectException"))) ;;at TAG_2
                                      (84 (dup)) 
                                      (85 (ldc 3)) ;;STRING:: "unexpected activator type"
                                      (87 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (90 (athrow)) 
                                      (91 (aload_1)) ;;at TAG_1
                                      (92 (aload_2)) 
                                      (93 (aload_1)) 
                                      (94 (invokeinterface (methodCP "getRefClass" "java.rmi.server.RemoteRef" ((class "java.io.ObjectOutput")) (class "java.lang.String")) 2)) 
                                      (99 (invokevirtual (methodCP "writeUTF" "java.io.ObjectOutputStream" ((class "java.lang.String")) void))) 
                                      (102 (aload_2)) 
                                      (103 (aload_1)) 
                                      (104 (invokeinterface (methodCP "writeExternal" "java.rmi.server.RemoteRef" ((class "java.io.ObjectOutput")) void) 2)) 
                                      (109 (return)) 
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 127)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "readObject" "java.io.ObjectInputStream" () (class "java.lang.Object")))) 
                                      (5 (checkcast (class "java.rmi.server.UID"))) 
                                      (8 (putfield (fieldCP "uid" "java.rmi.activation.ActivationID" (class "java.rmi.server.UID")))) 
                                      (11 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (14 (dup)) 
                                      (15 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (18 (ldc 4)) ;;STRING:: "sun.rmi.server."
                                      (20 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (23 (aload_1)) 
                                      (24 (invokevirtual (methodCP "readUTF" "java.io.ObjectInputStream" () (class "java.lang.String")))) 
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (30 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (33 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (36 (ldc_w )) 
                                      (39 (invokevirtual (methodCP "asSubclass" "java.lang.Class" ((class "java.lang.Class")) (class "java.lang.Class")))) 
                                      (42 (astore_2)) 
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "newInstance" "java.lang.Class" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "java.rmi.server.RemoteRef"))) 
                                      (50 (astore_3)) 
                                      (51 (aload_3)) 
                                      (52 (aload_1)) 
                                      (53 (invokeinterface (methodCP "readExternal" "java.rmi.server.RemoteRef" ((class "java.io.ObjectInput")) void) 2)) 
                                      (58 (aload_0)) 
                                      (59 (aconst_null)) 
                                      (60 (iconst_1)) 
                                      (61 (anewarray (class "java.lang.Class"))) 
                                      (64 (dup)) 
                                      (65 (iconst_0)) 
                                      (66 (ldc_w )) 
                                      (69 (aastore)) 
                                      (70 (new (class "java.rmi.server.RemoteObjectInvocationHandler"))) 
                                      (73 (dup)) 
                                      (74 (aload_3)) 
                                      (75 (invokespecial (methodCP "<init>" "java.rmi.server.RemoteObjectInvocationHandler" ((class "java.rmi.server.RemoteRef")) void))) 
                                      (78 (invokestatic (methodCP "newProxyInstance" "java.lang.reflect.Proxy" ((class "java.lang.ClassLoader") (array (class "java.lang.Class")) (class "java.lang.reflect.InvocationHandler")) (class "java.lang.Object")))) 
                                      (81 (checkcast (class "java.rmi.activation.Activator"))) 
                                      (84 (putfield (fieldCP "activator" "java.rmi.activation.ActivationID" (class "java.rmi.activation.Activator")))) 
                                      (87 (goto 126)) ;;to TAG_0;;at TAG_2
                                      (90 (astore_2)) ;;at TAG_3
                                      (91 (new (class "java.io.InvalidObjectException"))) 
                                      (94 (dup)) 
                                      (95 (ldc 5)) ;;STRING:: "Unable to create remote reference"
                                      (97 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (100 (aload_2)) 
                                      (101 (invokevirtual (methodCP "initCause" "java.io.InvalidObjectException" ((class "java.lang.Throwable")) (class "java.lang.Throwable")))) 
                                      (104 (checkcast (class "java.io.IOException"))) 
                                      (107 (athrow)) 
                                      (108 (astore_2)) ;;at TAG_4
                                      (109 (new (class "java.io.InvalidObjectException"))) 
                                      (112 (dup)) 
                                      (113 (ldc 5)) ;;STRING:: "Unable to create remote reference"
                                      (115 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (118 (aload_2)) 
                                      (119 (invokevirtual (methodCP "initCause" "java.io.InvalidObjectException" ((class "java.lang.Throwable")) (class "java.lang.Throwable")))) 
                                      (122 (checkcast (class "java.io.IOException"))) 
                                      (125 (athrow)) 
                                      (126 (return)) ;;at TAG_0
                                      (endofcode 127))
                                   (Exceptions 
                                     (handler 11 87  90 (class "java.lang.InstantiationException"))
                                     (handler 11 87  108 (class "java.lang.IllegalAccessException")))
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ActivationID-class-table*
  (make-static-class-decls 
   *java.rmi.activation.ActivationID*))

(defconst *package-name-map* 
  ("java.rmi.activation.ActivationID" . "java.rmi.activation"))

