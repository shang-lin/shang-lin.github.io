;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;CONSTRUCT.LSP
;;;CS20c Project: Sorting and Searching Trees
;;;Shang-Lin Chen (shang@cs.caltech.edu)
;;;last modified May 20, 2001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Loading this file returns the time taken to construct a binary and a ternary search tree
;;;with 10, 100, and 1000 nodes using randomly-generated 5-character strings.

;;construct a 10 node tree
(setq a10 (n-data-cells 10 5 70 79))
(setq tst-time0 (get-universal-time))
(dotimes (x 10000 x)
  (ts-tree-from-a-list a10))
(setq tst10-time (/ (- (get-universal-time) tst-time0) 10000))
(setq bst-time0 (get-universal-time))
(dotimes (x 10000 x)
  (bs-tree-from-a-list a10))
(setq bst10-time (/ (- (get-universal-time) bst-time0) 10000))
(format T "To construct a 10 node tree ~%")
(format T "BST ~A seconds  TST ~A seconds ~%" BST10-time tst10-time)


;;construct a 100 node tree
(setq a100 (n-data-cells 100 5 70 79))
(setq tst-time0 (get-universal-time))
(dotimes (x 5000 x)
  (ts-tree-from-a-list a100))
(setq tst100-time (/ (- (get-universal-time) tst-time0) 5000))
(setq bst-time0 (get-universal-time))
(dotimes (x 500 x)
  (bs-tree-from-a-list a100))
(setq bst100-time (/ (- (get-universal-time) bst-time0) 500))
(format T "To construct a 100 node tree ~%")
(format T "BST ~A seconds  TST ~A seconds~%" BST100-time tst100-time)


;;construct a 1000 node tree
(setq a1000 (n-data-cells 1000 5 70 79))
(setq tst-time0 (get-universal-time))
(dotimes (x 50 x)
  (ts-tree-from-a-list a1000))
(setq tst1000-time (/ (- (get-universal-time) tst-time0) 50))
(setq bst-time0 (get-universal-time))
(dotimes (x 50 x)
  (bs-tree-from-a-list a1000))
(setq bst1000-time (/ (- (get-universal-time) bst-time0) 50))
(format T "To construct a 1000 node tree ~%")
(format T "BST ~A seconds  TST ~A seconds ~%" bst1000-time tst1000-time)
