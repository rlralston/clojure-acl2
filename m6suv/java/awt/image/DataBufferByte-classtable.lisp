; DataBufferByte-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.image.DataBufferByte*
 (make-class-def
      '(class "java.awt.image.DataBufferByte"
            "java.awt.image.DataBuffer"
            (constant_pool)
            (fields
                        (field "data" (array byte) (accessflags  *class* ) -1)
                        (field "bankdata" (array (array byte)) (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "STABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_0))
                                      (5 (iload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int) void)))
                                      (9 (aload_0))
                                      (10 (iload_1))
                                      (11 (newarray BYTE))
                                      (13 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (16 (aload_0))
                                      (17 (iconst_1))
                                      (18 (anewarray (array byte)))
                                      (21 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (28 (iconst_0))
                                      (29 (aload_0))
                                      (30 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (33 (aastore))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "STABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_0)) 
                                      (5 (iload_1)) 
                                      (6 (iload_2)) 
                                      (7 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int) void))) 
                                      (10 (aload_0)) 
                                      (11 (iload_2)) 
                                      (12 (anewarray (array byte))) 
                                      (15 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte))))) 
                                      (18 (iconst_0)) 
                                      (19 (istore_3)) 
                                      (20 (iload_3)) ;;at TAG_1
                                      (21 (iload_2)) 
                                      (22 (if_icmpge 40))  ;;to TAG_0
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte))))) 
                                      (29 (iload_3)) 
                                      (30 (iload_1)) 
                                      (31 (newarray BYTE)) 
                                      (33 (aastore)) 
                                      (34 (iinc 3 1)) 
                                      (37 (goto 20)) ;;to TAG_1
                                      (40 (aload_0)) ;;at TAG_0
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte))))) 
                                      (45 (iconst_0)) 
                                      (46 (aaload)) 
                                      (47 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte)))) 
                                      (50 (return)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array byte) int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_0))
                                      (5 (iload_2))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int) void)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (14 (aload_0))
                                      (15 (iconst_1))
                                      (16 (anewarray (array byte)))
                                      (19 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (26 (iconst_0))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (31 (aastore))
                                      (32 (return))
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_0))
                                      (5 (iload_2))
                                      (6 (iconst_1))
                                      (7 (iload_3))
                                      (8 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int int) void)))
                                      (11 (aload_0))
                                      (12 (aload_1))
                                      (13 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (16 (aload_0))
                                      (17 (iconst_1))
                                      (18 (anewarray (array byte)))
                                      (21 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (28 (iconst_0))
                                      (29 (aload_0))
                                      (30 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (33 (aastore))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (array byte)) int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_0))
                                      (5 (iload_2))
                                      (6 (aload_1))
                                      (7 (arraylength))
                                      (8 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int) void)))
                                      (11 (aload_0))
                                      (12 (aload_1))
                                      (13 (invokevirtual
					(methodCP "clone" "byte[][]" () (class "java.lang.Object"))))
                                      (16 (checkcast (array (array byte))))
                                      (19 (checkcast (array (array byte))))
                                      (22 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (25 (aload_0))
                                      (26 (aload_0))
                                      (27 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (30 (iconst_0))
                                      (31 (aaload))
                                      (32 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (35 (return))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (array byte)) int (array int))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_0))
                                      (5 (iload_2))
                                      (6 (aload_1))
                                      (7 (arraylength))
                                      (8 (aload_3))
                                      (9 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int (array int)) void)))
                                      (12 (aload_0))
                                      (13 (aload_1))
                                      (14 (invokevirtual
					(methodCP "clone" "byte[][]" () (class "java.lang.Object"))))
                                      (17 (checkcast (array (array byte))))
                                      (20 (checkcast (array (array byte))))
                                      (23 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (26 (aload_0))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (31 (iconst_0))
                                      (32 (aaload))
                                      (33 (putfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (36 (return))
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getData"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferByte" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getData"
                              (parameters int)
                              (returntype . (array byte))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferByte" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (11 (iload_1))
                                      (12 (aaload))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBankData"
                              (parameters )
                              (returntype . (array (array byte)))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferByte" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (11 (invokevirtual
					(methodCP "clone" "byte[][]" () (class "java.lang.Object"))))
                                      (14 (checkcast (array (array byte))))
                                      (17 (checkcast (array (array byte))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getElem"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (4 (iload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "offset" "java.awt.image.DataBufferByte" int)))
                                      (9 (iadd))
                                      (10 (baload))
                                      (11 (sipush 255))
                                      (14 (iand))
                                      (15 (ireturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getElem"
                              (parameters int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (4 (iload_1))
                                      (5 (aaload))
                                      (6 (iload_2))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "offsets" "java.awt.image.DataBufferByte" (array int))))
                                      (11 (iload_1))
                                      (12 (iaload))
                                      (13 (iadd))
                                      (14 (baload))
                                      (15 (sipush 255))
                                      (18 (iand))
                                      (19 (ireturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setElem"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "data" "java.awt.image.DataBufferByte" (array byte))))
                                      (4 (iload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "offset" "java.awt.image.DataBufferByte" int)))
                                      (9 (iadd))
                                      (10 (iload_2))
                                      (11 (i2b))
                                      (12 (bastore))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferByte" (class "sun.java2d.StateTrackableDelegate"))))
                                      (17 (invokevirtual
					(methodCP "markDirty" "sun.java2d.StateTrackableDelegate" () void)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setElem"
                              (parameters int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferByte" (array (array byte)))))
                                      (4 (iload_1))
                                      (5 (aaload))
                                      (6 (iload_2))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "offsets" "java.awt.image.DataBufferByte" (array int))))
                                      (11 (iload_1))
                                      (12 (iaload))
                                      (13 (iadd))
                                      (14 (iload_3))
                                      (15 (i2b))
                                      (16 (bastore))
                                      (17 (aload_0))
                                      (18 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferByte" (class "sun.java2d.StateTrackableDelegate"))))
                                      (21 (invokevirtual
					(methodCP "markDirty" "sun.java2d.StateTrackableDelegate" () void)))
                                      (24 (return))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DataBufferByte-class-table*
  (make-static-class-decls 
   *java.awt.image.DataBufferByte*))

(defconst *package-name-map* 
  ("java.awt.image.DataBufferByte" . "java.awt.image"))
