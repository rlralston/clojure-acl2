#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../java/lang/Number")
(include-book "signed-big-endian")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "ACL2")

(defun post_add (x0 y0 result xIdx yIdx)
  (and (zp xIdx)
       (zp yIdx)          
       (equal (+ (sbe-to-nat-w x0) (sbe-to-nat-w y0))       
              (sbe-to-nat-w result))))#|ACL2s-ToDo-Line|#


(defun R3 (x0 y0 result xIdx yIdx) 
  

(in-package "MC")

(defconst *java.math.BigInteger*
  (make-class-decl
   "java.math.BigInteger"  ; class name
   '("java.lang.Number" "java.lang.Object")   ; superclass
   ; interfaces
   '()
   ; fields
   '()           
   ; static fields                       
   '()     
   ; constant pool (Seems to be numbered by index)
   '()                               
   (list ; methods
    '("add_ia.ia" (|INT[]| |INT[]|)

      ;  if (x.length < y.length) {
      ;      int[] tmp = x;
      ;      x = y;
      ;      y = tmp;
      ;  }
                  
      (aload_0)                        ; 0: aload_0
      (arraylength)                    ; 1: arraylength
      (aload_1)                        ; 2: aload_1
      (arraylength)                    ; 3: arraylength
      (if_icmpge 9)                    ; 4: if_icmpge 13
      (aload_0)                        ; 7: aload_0
      (astore_2)                       ; 8: astore_2
      (aload_1)                        ; 9: aload_1
      (astore_0)                       ; 10: astore_0
      (aload_2)                        ; 11: aload_2
      (astore_1)                       ; 12: astore_1
      
      ;  int xIndex = x.length;
      ;  int yIndex = y.length;
      ;  int result[] = new int[xIndex];
      ;  long sum = 0;
      
      (aload_0)                        ; 13: aload_0
      (arraylength)                    ; 14: arraylength
      (istore_2)                       ; 15: istore_2
      (aload_1)                        ; 16: aload_1
      (arraylength)                    ; 17: arraylength
      (istore_3)                       ; 18: istore_3
      (iload_2)                        ; 19: iload_2
      (newarray)                       ; 20: newarray int
      (astore 4)                       ; 22: astore 4
      (lconst_0)                       ; 24: lconst_0
      (lstore 5)                       ; 25: lstore 5
      
      ;  while(yIndex > 0) {
      ;      sum = (x[--xIndex] & LONG_MASK) +
      ;            (y[--yIndex] & LONG_MASK) + (sum >>> 32);
      ;      result[xIndex] = (int)sum;
      ;  }
      
      (iload_3)                        ; 27: iload_3
      (ifle 44)                        ; 28: ifle 72
      (aload_0)                        ; 31: aload_0
      (iinc 2 -1)                      ; 32: iinc 2, -1
      (iload_2)                        ; 35: iload_2
      (iaload)                         ; 36: iaload
      (i2l)                            ; 37: i2l
      (ldc2_w 0)                       ; 38: ldc2_w        #437                // long 4294967295l
      (land)                           ; 41: land
      (aload_1)                        ; 42: aload_1
      (iinc 3 -1)                      ; 43: iinc 3, -1
      (iload_3)                        ; 46: iload_3
      (iaload)                         ; 47: iaload
      (i2l)                            ; 48: i2l
      (ldc2_w 0)                       ; 49: ldc2_w        #437                // long 4294967295l
      (land)                           ; 52: land
      (ladd)                           ; 53: ladd
      (lload 5)                        ; 54: lload 5
      (bipush 32)                      ; 56: bipush 32
      (lushr)                          ; 58: lushr
      (ladd)                           ; 59: ladd
      (lstore 5)                       ; 60: lstore 5
      (aload 4)                        ; 62: aload 4
      (iload_2)                        ; 64: iload_2
      (lload 5)                        ; 65: lload 5
      (l2i)                            ; 67: l2i
      (iastore)                        ; 68: iastore
      (goto -42)                       ; 69: goto 27
      
      ;  boolean carry = (sum >>> 32 != 0);
      
      (lload 5)                        ; 72: lload 5
      (bipush 32)                      ; 74: bipush 32
      (lushr)                          ; 76: lushr
      (lconst_0)                       ; 77: lconst_0
      (lcmp)                           ; 78: lcmp
      (ifeq 7)                         ; 79: ifeq 86
      (iconst_1)                       ; 82: iconst_1 (boolean true)
      (goto 4)                         ; 83: goto 87
      (iconst_0)                       ; 86: iconst_0 (boolean false)     
      (istore 7)                       ; 87: istore 7
      
      ;  while (xIndex > 0 && carry)
      ;      carry = ((result[--xIndex] = x[xIndex] + 1) == 0);      
      
      (iload_2)                        ; 89: iload_2
      (ifle 34)                        ; 90: ifle 124
      (iload 7)                        ; 93: iload 7
      (ifeq 29)                        ; 95: ifeq 124
      (aload 4)                        ; 98: aload 4
      (iinc 2 -1)                      ; 100: iinc 2, -1
      (iload_2)                        ; 103: iload_2
      (aload_0)                        ; 104: aload_0
      (iload_2)                        ; 105: iload_2
      (iaload)                         ; 106: iaload
      (iconst_1)                       ; 107: iconst_1
      (iadd)                           ; 108: iadd
      (dup_x2)                         ; 109: dup_x2
      (iastore)                        ; 110: iastore
      (ifne 7)                         ; 111: ifne 118
      (iconst_1)                       ; 114: iconst_1
      (goto 4)                         ; 115: goto 119
      (iconst_0)                       ; 118: iconst_0
      (istore 7)                       ; 119: istore 7
      (goto -32)                       ; 121: goto 89
      
      ;  while (xIndex > 0)
      ;      result[--xIndex] = x[xIndex];
      
      (iload_2)                        ; 124: iload_2
      (ifle 16)                        ; 125: ifle 141
      (aload 4)                        ; 128: aload 4
      (iinc 2 -1)                      ; 130: iinc 2, -1
      (iload_2)                        ; 133: iload_2
      (aload_0)                        ; 134: aload_0
      (iload_2)                        ; 135: iload_2
      (iaload)                         ; 136: iaload
      (iastore)                        ; 137: iastore
      (goto -14)                       ; 138: goto 124
      
      ; if (carry) {
      
      (iload 7)                        ; 141: iload 7
      (ifeq 32)                        ; 143: ifeq 175
      
      ;  int bigger[] = new int[result.length + 1];
      ;  System.arraycopy(result, 0, bigger, 1, result.length);
      ;  bigger[0] = 0x01;
      ;  return bigger;
      
      (aload 4)                        ; 146: aload 4
      (arraylength)                    ; 148: arraylength
      (iconst_1)                       ; 149: iconst_1
      (iadd)                           ; 150: iadd
      (newarray)                       ; 151: newarray int
      (astore 8)                       ; 153: astore 8
      (aload 4)                        ; 155: aload 4
      (iconst_0)                       ; 157: iconst_0
      (aload 8)                        ; 158: aload 8
      (iconst_1)                       ; 160: iconst_1
      (aload 4)                        ; 161: aload 4
      (arraylength)                    ; 163: arraylength
      (invokestatic "java.lang.System" ; 164: invokestatic  #809                // Method java/lang/system.arraycopy:(ljava/lang/object;iljava/lang/object;ii)v
                    "arraycopy" 
                    5)                   
      (aload 8)                        ; 167: aload 8
      (iconst_0)                       ; 169: iconst_0
      (iconst_1)                       ; 170: iconst_1
      (iastore)                        ; 171: iastore
      (aload 8)                        ; 172: aload 8
      (areturn)                        ; 174: areturn
      
      ;  return result;
      
      (aload 4)                        ; 175: aload 4      
      (areturn))                       ; 177: areturn
    )
   '(REF -1)))