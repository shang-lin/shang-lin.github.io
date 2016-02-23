;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SEARCH.LSP
;;;CS20c Project: Sorting and Searching Strings
;;;Shang-Lin Chen (shang@cs.caltech.edu)
;;;last modified June 14, 2001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Loading this file returns the time taken to search for 1 randomly generated
;;;string in binary and ternary search trees with n nodes, where n is 10, 100, or 1000.


(setq b500 (n-data-cells 500 5 70 79)) 
(setq s500 NIL)
(dolist (x b500 "all-done")
  (setq s500 (append s500 (list (reverse x)))))

;;search s500 with a TST tree of 10 nodes
(setq a10 (n-data-cells 10 5 70 79))
(setq tst10 (ts-tree-from-a-list a10))  ;construct a TST-tree of 10 nodes
(setq tst10-time0 (get-universal-time))
(dotimes (i 1000 i)
(dolist (x s500 "all-done")
  (SEARCH-TST (car x) tst10))   
(setq tst10-time (/ (- (get-universal-time) tst10-time0) 500000)))

;;search s500 with a BST tree of 10 nodes
(setq bst10 (bs-tree-from-a-list a10))  ;construct a BST-tree of 10 nodes
(setq bst10-time0 (get-universal-time))
(dotimes (i 1000 i)
(dolist (x s500 "all-done")
  (SEARCH-BST (car x) bst10))
(setq bst10-time (/ (- (get-universal-time) bst10-time0) 500000)))

;;search s500 with a TST tree of 100 nodes
(setq a100 (n-data-cells 100 5 70 79))
(setq tst100 (ts-tree-from-a-list a100))  ;construct a TST-tree of 100 nodes
(setq tst100-time0 (get-universal-time))
(dotimes (i 100 i)
(dolist (x s500 "all-done")
   (SEARCH-TST (car x) tst100)) ) 
(setq tst100-time (/ (- (get-universal-time) tst100-time0) 50000))

;;search s500 with a BST tree of 100 nodes
(setq bst100 (bs-tree-from-a-list a100))  ;construct a BST-tree of 100 nodes
(setq bst100-time0 (get-universal-time))
(dotimes (i 100 1)
(dolist (x s500 "all-done")
  (SEARCH-BST (car x) bst100)) )
(setq bst100-time (/ (- (get-universal-time) bst100-time0) 50000))


;;search s500 with a TST tree of 1000 nodes
(setq a1000 (n-data-cells 1000 5 70 79))
(setq tst1000 (ts-tree-from-a-list a1000))  ;construct a TST-tree of 1000 nodes
(setq tst1000-time0 (get-universal-time))

(dotimes (i 10 i)
(dolist (x s500 "all-done")
   (SEARCH-TST (car x) tst1000))  )
(setq tst1000-time (/ (- (get-universal-time) tst1000-time0) 5000))

;;search s500 with a BST tree of 1000 nodes
(setq bst1000 (bs-tree-from-a-list a1000)) ;construct a BST-tree of 1000 nodes
(setq bst1000-time0 (get-universal-time))
(dotimes (i 10 i)
(dolist (x s500 "all-done")
  (SEARCH-BST (car x) bst1000)) )
(setq bst1000-time (/ (- (get-universal-time) bst1000-time0) 5000))


(PRINT "Time to search a string of 5 characters in 10 node trees ~%")
(format T "BST-tree = ~A seconds   TST-tree = ~A seconds ~%" 
            bst10-time tst10-time)
(PRINT "Time to search a string of 5 characters in 100 node trees ~%")
(format T "BST-tree = ~A seconds   TST-tree = ~A seconds ~%" 
            bst100-time tst100-time)
(PRINT "Time to search a string of 5 characters in 1000 node trees %")
(format T "BST-tree = ~A seconds   TST-tree = ~A seconds ~%" 
            bst1000-time tst1000-time)

