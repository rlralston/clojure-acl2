; Activatable-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.activation.Activatable*
 (make-class-def
      '(class "java.rmi.activation.Activatable"
            "java.rmi.server.RemoteServer"
            (constant_pool
                        (LONG -3120617863591563455))
            (fields
                        (field "id" (class "java.rmi.activation.ActivationID") (accessflags  *class*  *private* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.rmi.server.RemoteServer" () void)))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (aload_2))
                                      (8 (iload_3))
                                      (9 (iload 4))
                                      (11 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int) (class "java.rmi.activation.ActivationID"))))
                                      (14 (putfield (fieldCP "id" "java.rmi.activation.Activatable" (class "java.rmi.activation.ActivationID"))))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 8) (max_locals . 7) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.rmi.server.RemoteServer" () void)))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (aload_2))
                                      (8 (iload_3))
                                      (9 (iload 4))
                                      (11 (aload 5))
                                      (13 (aload 6))
                                      (15 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory")) (class "java.rmi.activation.ActivationID"))))
                                      (18 (putfield (fieldCP "id" "java.rmi.activation.Activatable" (class "java.rmi.activation.ActivationID"))))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.rmi.activation.ActivationID") int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.rmi.server.RemoteServer" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "id" "java.rmi.activation.Activatable" (class "java.rmi.activation.ActivationID"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (iload_2))
                                      (12 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.rmi.activation.ActivationID") int) (class "java.rmi.Remote"))))
                                      (15 (pop))
                                      (16 (return))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.rmi.activation.ActivationID") int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.rmi.server.RemoteServer" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "id" "java.rmi.activation.Activatable" (class "java.rmi.activation.ActivationID"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (iload_2))
                                      (12 (aload_3))
                                      (13 (aload 4))
                                      (15 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.rmi.activation.ActivationID") int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory")) (class "java.rmi.Remote"))))
                                      (18 (pop))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getID"
                              (parameters )
                              (returntype . (class "java.rmi.activation.ActivationID"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "id" "java.rmi.activation.Activatable" (class "java.rmi.activation.ActivationID"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "register"
                              (parameters (class "java.rmi.activation.ActivationDesc"))
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getSystem" "java.rmi.activation.ActivationGroup" () (class "java.rmi.activation.ActivationSystem"))))
                                      (3 (aload_0))
                                      (4 (invokeinterface
					(methodCP "registerObject" "java.rmi.activation.ActivationSystem" ((class "java.rmi.activation.ActivationDesc")) (class "java.rmi.activation.ActivationID")) 2))
                                      (9 (astore_1))
                                      (10 (aload_0))
                                      (11 (aload_1))
                                      (12 (invokestatic
					(methodCP "getStub" "sun.rmi.server.ActivatableRef" ((class "java.rmi.activation.ActivationDesc") (class "java.rmi.activation.ActivationID")) (class "java.rmi.Remote"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "inactive"
                              (parameters (class "java.rmi.activation.ActivationID"))
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "currentGroup" "java.rmi.activation.ActivationGroup" () (class "java.rmi.activation.ActivationGroup"))))
                                      (3 (aload_0))
                                      (4 (invokevirtual
					(methodCP "inactiveObject" "java.rmi.activation.ActivationGroup" ((class "java.rmi.activation.ActivationID")) boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "unregister"
                              (parameters (class "java.rmi.activation.ActivationID"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getSystem" "java.rmi.activation.ActivationGroup" () (class "java.rmi.activation.ActivationSystem"))))
                                      (3 (aload_0))
                                      (4 (invokeinterface
					(methodCP "unregisterObject" "java.rmi.activation.ActivationSystem" ((class "java.rmi.activation.ActivationID")) void) 2))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "exportObject"
                              (parameters (class "java.rmi.Remote") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int)
                              (returntype . (class "java.rmi.activation.ActivationID"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (aconst_null))
                                      (7 (aconst_null))
                                      (8 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory")) (class "java.rmi.activation.ActivationID"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "exportObject"
                              (parameters (class "java.rmi.Remote") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory"))
                              (returntype . (class "java.rmi.activation.ActivationID"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 12) (code_length . 82)
                                   (parsedcode
                                      (0 (new (class "java.rmi.activation.ActivationDesc"))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (11 (aload_1)) 
                                      (12 (aload_2)) 
                                      (13 (iload_3)) 
                                      (14 (invokespecial (methodCP "<init>" "java.rmi.activation.ActivationDesc" ((class "java.lang.String") (class "java.lang.String") (class "java.rmi.MarshalledObject") boolean) void))) 
                                      (17 (astore 7)) 
                                      (19 (invokestatic (methodCP "getSystem" "java.rmi.activation.ActivationGroup" () (class "java.rmi.activation.ActivationSystem")))) 
                                      (22 (astore 8)) 
                                      (24 (aload 8)) 
                                      (26 (aload 7)) 
                                      (28 (invokeinterface (methodCP "registerObject" "java.rmi.activation.ActivationSystem" ((class "java.rmi.activation.ActivationDesc")) (class "java.rmi.activation.ActivationID")) 2)) 
                                      (33 (astore 9)) 
                                      (35 (aload_0)) ;;at TAG_2
                                      (36 (aload 9)) 
                                      (38 (iload 4)) 
                                      (40 (aload 5)) 
                                      (42 (aload 6)) 
                                      (44 (invokestatic (methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "java.rmi.activation.ActivationID") int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory")) (class "java.rmi.Remote")))) 
                                      (47 (pop)) 
                                      (48 (goto 70)) ;;to TAG_0;;at TAG_3
                                      (51 (astore 10)) ;;at TAG_4
                                      (53 (aload 8)) ;;at TAG_5
                                      (55 (aload 9)) 
                                      (57 (invokeinterface (methodCP "unregisterObject" "java.rmi.activation.ActivationSystem" ((class "java.rmi.activation.ActivationID")) void) 2)) 
                                      (62 (goto 67)) ;;to TAG_1;;at TAG_6
                                      (65 (astore 11)) ;;at TAG_7
                                      (67 (aload 10)) ;;at TAG_1
                                      (69 (athrow)) 
                                      (70 (invokestatic (methodCP "currentGroup" "java.rmi.activation.ActivationGroup" () (class "java.rmi.activation.ActivationGroup")))) ;;at TAG_0
                                      (73 (aload 9)) 
                                      (75 (aload_0)) 
                                      (76 (invokevirtual (methodCP "activeObject" "java.rmi.activation.ActivationGroup" ((class "java.rmi.activation.ActivationID") (class "java.rmi.Remote")) void))) 
                                      (79 (aload 9)) 
                                      (81 (areturn)) 
                                      (endofcode 82))
                                   (Exceptions 
                                     (handler 35 48  51 (class "java.rmi.RemoteException"))
                                     (handler 53 62  65 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "exportObject"
                              (parameters (class "java.rmi.Remote") (class "java.rmi.activation.ActivationID") int)
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "sun.rmi.server.ActivatableServerRef")))
                                      (4 (dup))
                                      (5 (aload_1))
                                      (6 (iload_2))
                                      (7 (invokespecial
					(methodCP "<init>" "sun.rmi.server.ActivatableServerRef" ((class "java.rmi.activation.ActivationID") int) void)))
                                      (10 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "sun.rmi.server.ActivatableServerRef")) (class "java.rmi.Remote"))))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "exportObject"
                              (parameters (class "java.rmi.Remote") (class "java.rmi.activation.ActivationID") int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory"))
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "sun.rmi.server.ActivatableServerRef")))
                                      (4 (dup))
                                      (5 (aload_1))
                                      (6 (iload_2))
                                      (7 (aload_3))
                                      (8 (aload 4))
                                      (10 (invokespecial
					(methodCP "<init>" "sun.rmi.server.ActivatableServerRef" ((class "java.rmi.activation.ActivationID") int (class "java.rmi.server.RMIClientSocketFactory") (class "java.rmi.server.RMIServerSocketFactory")) void)))
                                      (13 (invokestatic
					(methodCP "exportObject" "java.rmi.activation.Activatable" ((class "java.rmi.Remote") (class "sun.rmi.server.ActivatableServerRef")) (class "java.rmi.Remote"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "unexportObject"
                              (parameters (class "java.rmi.Remote") boolean)
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokestatic
					(methodCP "unexportObject" "sun.rmi.transport.ObjectTable" ((class "java.rmi.Remote") boolean) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "exportObject"
                              (parameters (class "java.rmi.Remote") (class "sun.rmi.server.ActivatableServerRef"))
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (instanceof (class "java.rmi.activation.Activatable"))) 
                                      (4 (ifeq 15))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (checkcast (class "java.rmi.activation.Activatable"))) 
                                      (11 (aload_1)) 
                                      (12 (putfield (fieldCP "ref" "java.rmi.activation.Activatable" (class "java.rmi.server.RemoteRef")))) 
                                      (15 (aload_1)) ;;at TAG_0
                                      (16 (aload_0)) 
                                      (17 (aconst_null)) 
                                      (18 (iconst_0)) 
                                      (19 (invokevirtual (methodCP "exportObject" "sun.rmi.server.ActivatableServerRef" ((class "java.rmi.Remote") (class "java.lang.Object") boolean) (class "java.rmi.Remote")))) 
                                      (22 (areturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Activatable-class-table*
  (make-static-class-decls 
   *java.rmi.activation.Activatable*))

(defconst *package-name-map* 
  ("java.rmi.activation.Activatable" . "java.rmi.activation"))
