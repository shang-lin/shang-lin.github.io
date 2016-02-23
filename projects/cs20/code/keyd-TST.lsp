;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;KEYD-TST.LSP
;;;CS20c Project: Sorting and Searching Strings
;;;Shang-Lin Chen (shang@cs.caltech.edu)
;;;last modified May 20, 2001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Algorithm for constructing and searching binary and ternary search trees

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;              LEXICAL CLOSURE FOR KEYED BINARY SEARCH TREE

(let ((KBS-tree NIL)
     )

 ;;FUNCTION KEYD-BST
 ;;To create a key-worded binary search tree. The inputto this function
 ;;is a list, and the first element of the input list should be a string
 ;;which serves as the keyword. CDR of the input list can be anything.
 ;;The storage order of the binary tree is totally determined by the
 ;;keyword.
 (defun KEYD-BST (x)
   (setq KBS-tree (K-ATTACH x KBS-tree)) )



 ;;FUNCTION K-ATTACH
 ;;To attach a node x to a partial tree y and return the result
 (defun K-ATTACH (x y)
   (let ((temp NIL) (xx (first x)) (yy (first (first y))) )
     ;explod the keywords to lists of characters
     (if (stringp xx) (setq xx (string->chars xx)))
     (if (stringp yy) (setq yy (string->chars yy))) 
     (COND
         ;if the partial tree is NIL, set up a new partial tree 
         ((null y)
          (setq temp (append (list x) '(NIL) '(NIL))) ) 

         ;if the length of a non-empty y is not 3, it is an input error
         ((NOT (EQUAL (length y) 3))
          (ERROR "K-ATTACH  a non-empty partial tree should have length of 3"))

         ;If keywords match, replace the old data with the new data
         ((EQUAL xx yy)
          (setq temp (append (list x) (cdr y))) )

         ;If the keyword of x is greater than that of y, go to the right branch
         ((K-GREATER xx yy)
          (setq temp (list (first y) (second y) 
                           (K-ATTACH x (third y)) )) )

         ;If the keyword of x is less than that of y, go to the left branch
         ((K-LESSTHAN xx yy)
          (setq temp (list (first y) (K-ATTACH x (second y)) (third y))) )

         ;Should not come to this default if everything is OK
         (T (ERROR "K-ATTACH error  should not come here")) ) ;end of COND
   ))


 ;;FUNCTION K-GREATER
 ;;To determine whether the keyword x is larger than the keyword y or not.
 (defun K-GREATER (x y)
   (let ((xx x) (yy y))
      ;explod the keywords into lists of characters, if they are not already so
      (if (stringp x) (setq xx (string->chars x)))
      (if (stringp y) (setq yy (string->chars y)))

      (COND
         ;if xx is NIL but yy is not, xx is less than yy 
         ((AND (null xx) (NOT (null yy))) NIl)

         ;if xx is not NIL but yy is NIL, xx is greater than yy
         ((AND (NOT (null xx)) (null yy)) T)

         ;if both xx and yy are NIL, they are equal
         ((AND (null xx) (null yy)) NIL)

         ;if the first characters of xx and yy are equal, go check the 
         ;remaining characters
         ((EQUAL (first xx) (first yy))
          (K-GREATER (cdr xx) (cdr yy)) )

         ;if first character of xx is less than first character of yy, 
         ;return NIL
         ((< (char-code (first xx)) (char-code (first yy))) NIL)

         ;if first character of xx is greater than first character of yy,
         ;return T
         ((> (char-code (first xx)) (char-code (first yy))) T)

         ;if everything is OK, should not come to this default
         (T (ERROR "K-GREATER something wrong  should not come here"))
      )))



 ;;FUNCTION K-LESSTHAN
 ;;To determine whether the keyword x is smaller than the keyword y or not.
 (defun K-LESSTHAN (x y)
   (let ((xx x) (yy y))
      ;explode keywords into lists of characters if they are not already so
      (if (stringp x) (setq xx (string->chars x)))
      (if (stringp y) (setq yy (string->chars y)))
      (COND

         ;if xx is NIL but yy is not, xx is less than yy
         ((AND (null xx) (NOT (null yy))) T)

         ;if xx is not NIL but yy is, xx is greater than yy
         ((AND (NOT (null xx)) (null yy)) NIL)

         ;if both xx and yy are NIL, they are equal
         ((AND (null xx) (null yy)) NIL)

         ;If the first char of xx equals the first char of yy,
         ;go to check the remaining characters in xx and yy.
         ((EQUAL (first xx) (first yy))
          (K-LESSTHAN (cdr xx) (cdr yy)) )

         ;if the first char of xx is greater than that of yy, return NIL
         ((> (char-code (first xx)) (char-code (first yy))) NIL)

         ;if the first char of xx is less than that of yy, return T
         ((< (char-code (first xx)) (char-code (first yy))) T)

         ;if everything is OK, should not come to this default
         (T (ERROR "K-GREATER something wrong  should not come here"))
      )))

 ;;FUNCTION STRING->CHARS
 ;;Convert a string to a flat list of characters
 (defun STRING->CHARS (str)
   (coerce str 'list))
             


 ;;Utility Functions
 (defun GET-KBS-tree () KBS-tree)
 (defun K-RESET () (setq KBS-tree NIL))  
 (defun SET-KBST (z) (setq KBS-tree z))

) ;end of lexical closure


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     SUPPORTING FUNCTIONS OUTSIDE THE LEXICAL CLOSURE OF BST

;;FUNCTION SEARCH-BST
;;Given a keyword, and a tree, search for the node with the matching 
;;keyword in the binary search tree 
(defun SEARCH-BST (k-word a-tree)
  (let ((t-node NIL) (t-word NIL) 
       (yy (first (first a-tree))) )
 ;   (format T "is k-word a string? ~A ~%" (stringp k-word))
 ;   (read)

    (if (AND (NOT (listp k-word)) (NOT (stringp k-word)))
        (ERROR "Input to SEARCH-BST should be a string or a list"))
    (setq t-word (string->chars k-word))
    (if (stringp yy) (setq yy (string->chars yy)))
    (COND 
       ((null a-tree) (setq t-node NIL))
       ((EQUAL t-word yy)
        (setq t-node (first a-tree)) ) 
       ((K-GREATER t-word yy)
        (setq t-node (SEARCH-BST t-word (third a-tree))) )
       ((K-LessThan t-word yy)
        (setq t-node (SEARCH-BST t-word (second a-tree))) )
       (T))
    t-node))


;;FUNCTION APPEND-NODE 
;;Append a node to a given BS-tree
(defun APPEND-NODE (a-node a-tree)
  (let ((temp NIL) (t1 NIL))
    (setq t1 (GET-KBS-tree))   ;save the existing KBS-tree to t1
    (SET-KBST a-tree)     ;load a-tree into KBS-tree
    (KEYD-BST a-node)
    (setq temp (GET-KBS-tree))
    (SET-KBST t1)        ;restore KBS-tree to the original condition
    temp))     



;;FUNCTION BS-Tree-From-a-List
;;Return a binary search tree from an input list of randomly generated
;;data-cells
(defun BS-Tree-From-a-List (a-list)
  (let ((t1 NIL) (t2 NIL))
    (SET-KBST NIL)
    (mapcar #'(lambda (x)
       (KEYD-BST x)
    ) a-list)
    (GET-KBS-tree))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          LEXICAL CLOSURE OF A TENARY SEARCH TREE


(let ((TS-tree NIL) (scratch NIL))


 ;;FUNCTION TST
 ;;To create a tenary search tree
 ;;A terary search tree is a 4 element list
 ;;The first element is a list of length 2, where the first element
 ;;of the first element is a one-character list, and the second element
 ;;of the first element is for the data we want to store.
 ;;The second, the 3rd and the 4-th elements of a tenary search tree
 ;;themselves are tenary search trees.
 ;;The input to the function is a 2 element list; the first element is
 ;;a string serving as the keyword, and the second element is the data.
 ;;The input is in the same format with the binary search tree.
 (defun TST (x)
   (setq TS-tree (T-ATTACH x TS-tree)))


 ;;FUNCTION T-ATTACH
 ;;to attach a node x to a tenary search tree y
 ;;If the keyword of x is only one character long and match the one
 ;;character keyword of y, then the input data is stored in the data
 ;;storage area of y, that is the second element of the first element of
 ;;y. If something is already stored in y, the original data will be
 ;;overwritten. 
 ;;If the keyword of x is more than onecharacter long, but the first
 ;;character is equal to the one-character keyword of y, a new input
 ;;with the first character of the keyword of x removed, plus the 
 ;;input data is passed to the first tenary tree. 
 ;;If the first character of the keyword of x is lesser (greater) than
 ;;the one-character key of y, x is passed to the second (third) 
 ;;sub tenary tree.
 (defun T-ATTACH (x y)
 (let ((temp NIL) (xx (first x)) (yy (first (first y))) (x1 NIL))
   ;explode the keywords to lists of characters
   (COND ((stringp xx) (setq xx (string->chars xx)))
         ((stringp yy) (setq yy (string->chars yy)))
         ((NOT (AND (listp xx) (listp yy)))
          (ERROR "Keyword of x should be a string or a flat list
                  of characters") )
         (T))  ;end of COND
;   (format T "xx= ~A yy= ~A ~%" xx yy)
;   (read)

   (COND
     ;if the tree y is nil
     ((NULL y)
      (COND ((EQUAL (length xx) 1)
             (setq temp (list x NIL NIL NIL)))
            (T
              (setq temp (list 
                             (list (list (first xx)) NIL) 
                             NIL NIL NIL))
              (setq temp (T-ATTACH x temp)) )) )
     ;If the length of the tree is not equal to 4
     ((NOT (EQUAL (length y) 4))
      (ERROR "A non-empty tenary tree y should have length 4"))
 
     ;keyword of x is one character long and equal to that of y  
     ((EQUAL xx yy)
      (setq temp (append (list x) (cdr y))) )

     ;Keyword of x is more than one character long, but the first
     ;character equals the keyword of y. 
     ;Remove one character from the keyword of x and attach to the
     ;middle-sub-tree (the 2nd element) of y
     ((AND (> (length xx) 1) (EQUAL (first xx) (first yy)))
      (setq x1 (list (cdr xx) (car (cdr x))))
      (setq temp (list (first y) (T-ATTACH x1 (second y)) (third y)
                       (nth 3 y))) )
     ;First element of keyword of x is greater than the keyword of y
     ;Attach x to the right-sub-tree (the 4-th element) of y
     ((T-GREATER xx yy)
      (setq temp (list (first y) (second y) (third y)
                       (T-ATTACH x (nth 3 y)))) )

     ;First element of the keyword of x is less than the keyword of y
     ;Attach x to the left-sub-tree (the 3rd element) of y
     ((T-LESSER xx yy)
      (setq temp (list (first y) (second y)
                       (T-ATTACH x (third y)) (nth 3 y))) )
     ;
     (T
       (ERROR "Should not come to here in T-ATTACH") )) ;end of COND
     temp))


;;FUNCTION T-GREATER (x y)
;;x and y must be flat lists. Compare the first element of x and y
;;to see that of x is greater than that of y
(defun T-GREATER (x y)
  (let ((temp NIL))
    (if (> (char-code (first x)) (char-code (first y)))
        (setq temp T))
    temp))

;;FUNCTION T-LESSER (x y)
(defun T-LESSER (x y)
  (let ((temp NIL))
    (if (< (char-code (first x)) (char-code (first y)))
        (setq temp T))
    temp))       


;;Utility Functions
(defun GET-TS-tree () TS-tree)
(defun T-RESET () (setq TS-tree NIL) (setq scratch NIL))
(defun SET-TS-tree (z) (setq TS-tree z))
(defun GET-SCRATCH () scratch)
(defun RESET-SCRATCH () (setq scratch NIL))
(defun SET-SCRATCH (z) (setq scratch z))
) ;end of the lexical closure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              OUTSIDE FUNCTIONS OF LEXICAL CLOSURE OF TST

;;FUNCTION SEARCH-TST
;;Given a keyword, and a tree, search for the node with the matching
;;keyword in the tenary search tree
(defun SEARCH-TST (k-word a-tree)
  (let ((t-tree NIL) (t-word NIL) (t-node NIL) 
       (yy (first (first a-tree))))
    (if (NOT (stringp k-word))
        (ERROR "Input to SEARCH-TST should be a string"))
    (RESET-SCRATCH)
    (setq t-word (string->chars k-word))
    (if (stringp yy) (setq yy (string->chars yy)))
    (setq t-tree (append (list (append (list yy) (cdr (first a-tree))))
                         (cdr a-tree)))
    (RECURSION-SEARCH-TST t-word t-tree) ))


(defun RECURSION-SEARCH-TST (t-word a-tree)
  (let ((t-tree NIL) (t-node NIL) (t1 NIL)
        (yy (first (first a-tree))) )    
    (COND
      ((null a-tree) (setq t-node NIL))
      ((EQUAL t-word yy)
       (setq t1 (GET-SCRATCH))
       (setq t1 (append t1 (list t-word)))      
       (SET-SCRATCH t1)
       (setq t-node (append (list t1) (cdr (first a-tree)))) )
      ((AND (> (length t-word) 1) (EQUAL (first t-word) (first yy)))
       (setq t1 (GET-SCRATCH))
       (setq t1 (append t1 (list (first yy))))
       (SET-SCRATCH t1)
       (setq t-node 
             (RECURSION-SEARCH-TST (cdr t-word) (second a-tree))) )
      ((T-GREATER t-word yy)
       (setq t-node (RECURSION-SEARCH-TST t-word (nth 3 a-tree))) )
      ((T-LESSER t-word yy)
       (setq t-node (RECURSION-SEARCH-TST t-word (third a-tree))) )
      (T (ERROR "Should not come here in RECURSION-SEARCH-TST")) 
     )  ;end of COND
     t-node))


;;FUNCTION TS-Tree-From-a-List
;;Return a tenary search tree from an input list of randomly generated
;;data-cells
(defun TS-Tree-From-a-List (a-list)
  (let ((t1 NIL) (t2 NIL))
    (T-RESET)
    (mapcar #'(lambda (x)
       (TST x)
    ) a-list)
    (GET-TS-tree))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Generae Random Data Cells

;;FUNCTION Random-char-list
;;Generate a list of randomly generated ASCII characters
;;Input n1 specifies number of characters need to be randomly generated.
;;ASCII codes of the generated characters are greater or equal to
;;nstart but less or equal to nend.
(defun Random-char-list (n1 nstart nend)
  (let ((nn 0) (temp NIL))
    (setq nn (- nend nstart))
    (if (< nn 0) (ERROR "nend should be larger than nstart"))
    (dotimes (x n1 x)
      (setq t1 (code-char (+ nstart (random nn))))
      (setq temp (append temp (list t1)))
    )  ;end dotimes
   temp))



;;FUNCTION n-data-cells
;;Return a list of n data-cells, where each data-cell is a list of
;;length 2 with the first element a string of characters generated
;;by the function Random-char-list, and the second is the reverse
;;of the string
(defun n-data-cells (n n1 nstart nend)
  (let ((temp NIL) (t1 NIL) (t2 NIL))
    (dotimes (x n x)
      (setq t1 (Random-char-list n1 nstart nend))
      (setq t2 (list (concatenate 'string t1)
                     (concatenate 'string (reverse t1))))
      (setq temp (append temp (list t2)))
    )  ;end of dotimes
   temp)) 

