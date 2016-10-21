#|
CS480 Summer 2016 
Alyssa Evers, Mark McCaskey
Dr. Duric
Final Project
|#


;; Utilities
                                        ; Returns a list from 0 to n
(defun range (n)
  (loop for x from 0 to (1- n)
     collect x))

                                        ; Returns a partially applied function, that is a function 
                                        ; that defers some of its arguments for a later application
(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))

                                        ; Returns the function with its arguments flipped
(defun flip (f)
  (lambda (&rest args)
    (apply f (reverse args))))


(defparameter *50-states*
  '(
    (AL (GA FL MS TN))             ; AL = Alabama
    (AK ())                        ; AK = Alaska
    (AZ (CA NV UT CO NM))          ; AZ = Arizona
    (AR (TX OK MO TN MS LA))       ; AR = Arkansas
    (CA (OR NV AZ))                ; CA = California
    (CO (NM AZ UT WY NE KS OK))    ; CO = Colorado
    (CT (RI NY MA))                ; CT = Conneticut
    (DE (MD PA NJ))                ; DE = Delaware
    (DC (MD VA))                   ; DC = D.C.
    (FL (GA AL))                   ; FL = Florida
    (GA (SC NC TN AL FL))          ; GA = Georgia
    (HI ())                        ; HI = Hawaii
    (ID (WA OR NV UT WY MT))       ; ID = Idaho
    (IL (WI IA MO KY IN))          ; IL = Illinois
    (IN (IL KY OH MI))             ; IN = Indiana
    (IA (MN SD NE MO IL WI))       ; IA = Iowa
    (KS (CO OK MO NE))             ; KS = Kansas
    (KY (MO TN VA WV OH IN IL))    ; KY = Kentucky
    (LA (TX AR MS))                ; LA = Lousiana
    (ME (NH))                      ; ME = Maine
    (MD (DE PA WV DC VA))          ; MD = Maryland
    (MA (RI CT NY VT NH))          ; MA = Mass
    (MI (OH IN WI))                ; MI = Michigan
    (MN (WI IA SD ND))             ; MN = Minnesota
    (MS (LA AR TN AL))             ; MS = Mississippi
    (MO (KS NE IA IL KY TN AR OK)) ; MO = Missouri
    (MT (ID WY SD ND))             ; MT = Montana
    (NE (WY SD IA MO KS CO))       ; NE = Nebraska
    (NV (CA OR ID UT AZ))          ; NV = Nevada
    (NH (ME MA VT))                ; NH = New Hampshire
    (NJ (NY PA DE))                ; NJ = New Jersey
    (NM (AZ UT CO OK TX))          ; NM = New Mexico
    (NY (PA NJ CT MA VT))          ; NY = New York
    (NC (VA TN GA SC))             ; NC = North Carolina
    (ND (MT SD MN))                ; ND = North Dakota
    (OH (PA WV KY IN MI))          ; OH = Ohio
    (OK (TX NM CO KS MO AR))       ; OK = Oklahoma
    (OR (WA ID NV CA))             ; OR = Oregon
    (PA (NY NJ DE MD WV OH))       ; PA = Pennsylvania
    (RI (CT MA))                   ; RI = Rhode Island
    (SC (GA NC))                   ; SC = South Carolina
    (SD (WY MT ND MN IA NE))       ; SD = South Dakota
    (TN (AR MO KY VA NC GA AL MS)) ; TN = Tennessee
    (TX (NM OK AR LA))             ; TX = Texas
    (UT (CO NM AZ NV ID WY))       ; UT = Utah
    (VT (NY MA NH))                ; VT = Vermont
    (VA (NC TN KY WV MD DC))       ; VA = Virginia
    (WA (ID OR))                   ; WA = Washington
    (WV (KY OH PA MD VA))          ; WV = West Virginia
    (WI (MN IA  IL MI))            ; WI = Wisconsin
    (WY (ID MT SD NE CO UT))))     ; WY = Wyoming


(defparameter australia '((wa  (nt sa))
                          (nt  (wa sa ql))
                          (sa  (wa nt ql nsw vc))
                          (ql  (nt sa nsw))
                          (nsw (ql sa vc))
                          (vc  (sa nsw))
                          (ta  nil)))

(defparameter 3-colors '(R G B))
(defparameter 4-colors '(R G B Y))

(defparameter node-removal-test-map
  '((a (b c d)) (b (a c d)) (c (c d e)) (d (c b)) (e (c))))

#|

The graphs from the assignment will all be converted into
weighted graphs of the form:
((vertex1 (adjacent_vertex1 adjacent_vertex1 etc) weight)
 (vertex2 (adjacent_vertex1 adjacent_vertex1 etc) weight)
 etc)

and are informally referred to as weighted alist graphs.
Each sublist is referred to as just an alist since its just
an association list.

|#

(defun agraph->awgraph (graph)
  "Creates the weighted alist graph from the regular 
   alist graph given for the assignment"
  (loop for alist in graph
     collect (append alist (list 1))))

(defun weight-graph (graph colors)
 (mapcar
  #'(lambda (x) (weight-vertex x colors)) graph))

(defun weight-vertex (alist colors)
  (list (vertex alist)
	(adj-list alist)
	(cond ((zerop (length colors)) 0)
	      ((zerop (degree alist)) :infinite)
	      (t (/ (log (length colors))
		    (degree alist))))))

(defun sort-by-weight (graph)
  (let ((g (copy-alist graph)))
    (sort g #'(lambda (a b)
		(cond ((eq (weight a) :infinite) nil)
		      ((not (eq (weight b) :infinite))
		       (< (weight a) (weight b)))
		      (t t))))))

(defun awgraphp (graph)
  "Checks if some adjacency list is a weighted alist graph"
  (declare (ignore graph)))

(defun vertex (alist)
  "Returns the vertex of an alist in a weighted alist graph"
  (car alist))

(defun adj-list (alist)
  "Returns the weight of an alist in a weighted alist graph"
  (cadr alist))

(defun weight (alist)
  "Returns the weight of an alist in a weighted alist graph"
  (caddr alist))

(defun degree (alist)
  "Returns the number of incident nodes, that is
   the number of edges protruding from the vertex"
  (length (adj-list alist)))

(defun low-degree (alist)
  "True on vertices that are 0 or 1 degree, referred to as
   islands and leaves from now on."
  (<= (degree alist) 1))

(defun remove-nodes (graph nodes)
  (loop for alist in graph
     collect (list (vertex alist)
                   (set-difference (adj-list alist) nodes)
                   (weight alist))))


(defun prune-pred (graph pred)
  "Removes vertices and edges that satisfy the pred"
  (let ((invalids (mapcar #'car (remove-if-not pred graph)))
	(newgraph (remove-if pred graph)))
    (remove-nodes newgraph invalids)))

(defun prune-nodes (graph nodes)
  "Removes vertices and edges that satisfy the pred"
  (prune-pred graph #'(lambda (x)
                        (member (vertex x) nodes))))

(defun prune-node (graph node)
  (prune-nodes graph (list (vertex node))))

(defun prune-leaves (graph)
  "Removes vertices and edges that have degree 1 or 0"
  (prune-pred graph #'low-degree))

(defun sort-by (lst f)
  (sort lst #'(lambda (x y)
                (< (funcall f x)
                   (funcall f y)))))

(defun topo-sort (graph)
  "Sorts by weight / degree. In the case of weight 1 it sorts
   by largest degree first"
  (let ((g (copy-alist graph)))
    (sort-by g #'degree)))

(defun take-while (pred lst)
  (cond
    ((null lst)                  nil)
    ((funcall pred (first lst))  (cons (first lst)
                                       (take-while pred (rest lst))))
    (t                           (take-while pred (rest lst)))))

(defun has-leafp (graph)
  (not (null (find-if #'low-degree graph))))

(defun purge-leaves (graph)
  (let ((g (copy-alist graph)))
    (loop while (has-leafp g)
       do (setf g (prune-leaves g)))
    g))

(defun ga (graph)
  "Precondition: As of yet it it doesn't check if the graph is a 
                 weighted alist graph thus you must pass a 
                 weighted alist graph
   Returns: The cutset"
  (let ((cutset nil)
        (max nil)
	(g (purge-leaves (topo-sort graph))))
    (loop while (not (null g))
       do (setf g (topo-sort g))
         (setf max (first (last g)))
         (setf cutset (cons (vertex max) cutset))
         (setf g (prune-node g max))
         (setf g (purge-leaves g)))
    cutset))

(defun mga (graph)
  "Excepts weight graph for mga from weight-graph"
  (let ((cutset nil)
	(max nil)
	(g (adj-list-corrector (purge-leaves
				(sort-by-weight graph)) graph)))
    (loop while g
       do
	 (setf g (dont-care-weight-sort g)) 
	 (setf max (first g))
	 (setf cutset (adj-list-corrector
		       (cons (list (vertex max)
				   (adj-list max))
			     cutset) graph))
	 (setf g (prune-node g max))
	 (setf g (purge-leaves g)))
    ;;reverse cutset to remove cycle verts in order added
    (setf cutset (adj-list-corrector (reverse cutset) graph)) ;;index in order of assertion
    ;;ensure all possible verts have been removed
    (loop while 1
       do (if (tree-equal cutset
		  (remove-unnecessesary-cutset-verts
		   cutset graph))
	      (return cutset)
	      (setf cutset (remove-unnecessesary-cutset-verts
			    cutset graph))))))

(defun remove-unnecessesary-cutset-verts (cutset graph)
  (adj-list-corrector
   (loop for v in cutset
      append (if (test-vert-for-cycle-value graph cutset (vertex v))
		 nil
		 (list v))) graph))

(defun dont-care-weight-sort (graph)
  (mapcar #'(lambda (x) (list (vertex x) (adj-list x)))
	  (sort-by-weight (weight-graph graph 4-colors))))

(defun test-vert-for-cycle-value (maingraph graph vert) 
  (tree-equal (has-cycle? graph)
	      (has-cycle? (remove (assoc vert maingraph) maingraph))))

(defun adj-list-corrector (subgraph graph)
  (let ((relevant-nodes (mapcar #'car subgraph)))
    (loop for v in subgraph
       collect (list (car v)
		     (intersection (adj-list (assoc (car v) graph))
				   relevant-nodes)))))


(defun has-cycle? (graph &key (seen-nodes nil) (start-node (caar graph))
			   (ignore-node nil))
  "Returns tree of cycles"
  (if (null graph)
      nil
      (let ((current-adj (cadr (assoc start-node graph))))
	(if (intersection current-adj (remove ignore-node seen-nodes))
	    (list start-node (intersection current-adj (remove ignore-node seen-nodes)))
	    (let ((retval (loop for n in (remove ignore-node current-adj)
			     collect
			       (has-cycle?
				graph
				:seen-nodes (cons start-node seen-nodes)
				:start-node n
				:ignore-node start-node))))
	      (if (null (remove nil retval))
		  nil
		  (cons start-node (remove nil retval))))))))


(defun tree-from-cutset (graph cutset)
  (prune-nodes graph cutset))

(defun cutset-n-tree-graph (graph)
  (let* ((g (agraph->awgraph graph))
         (cutset (ga g)))
    (list cutset (tree-from-cutset g cutset))))

(defun awgraph->agraph (graph)
  (mapcar #'(lambda (x) (list (vertex x)
                              (adj-list x)))
          graph))

(defun prune-vertex (graph node)
  (prune-nodes graph (list node)))

(defun prune-color (cgraph vert color)
  ; Remove color from color possibility list of all neighbors
  ; then remove the node from the list and adjacency lists of all neighbors
  ; in the graph
  (prune-vertex (mapcar #'(lambda (x)
			    (if (member vert (adj-list x))
				(list (vertex x)
				      (adj-list x)
				      (remove color (caddr x)))
				x))
			cgraph)
		vert))


(defun color-graph2 (graph)
  "coloring functon.  Should be called from cc or mcc"
  (let ((ret-graph (copy-alist graph)))
    (loop for al in graph
       do (setf
	   ret-graph
	   (mapcar
	    #'(lambda (st)
		(if (member (vertex al)
			    (adj-list st))
		    (list (vertex st)
			  (adj-list st)
			  (remove
			   (caaddr
			    (assoc (vertex al)
				   ret-graph))
			   (caddr st)))
		    st))
	    ret-graph)))
    (if (notany #'caddr ret-graph)
	ret-graph
	nil)))

(defun color-forest (colored-forest)
  "break apart trees and color"
  (let* ((cf (reverse (topo-sort (copy-alist colored-forest))))
	 (trees (loop while (not (null cf))
		   collect (let ((rval (extract-first-tree cf)))
			     (setf cf (set-difference cf rval))
			     rval))))
    ;; we now have a list of trees
    (loop for tree in trees
       append (progn
		 (resolve-color-tree tree)))))

(defun resolve-color-tree (colored-tree &key (cur (car colored-tree)) (last-seen nil))
  "Takes a tree with multiple color values returns a tree with single color values"
  (cond
    ((null (caddr cur)) nil)
    ((null (remove last-seen (adj-list cur)))
     (list (list (vertex cur) (adj-list cur) (caaddr cur))))
    (t (loop named outer for c in (caddr cur)
	  do
	    (let ((child-colors
		   (loop for adj in (remove last-seen (adj-list cur))
		      append (let ((next (resolve-color-tree
					   colored-tree
					   :cur ((lambda (x)
						   (list (vertex x)
							 (adj-list x)
							 (remove c (caddr x))))
						 (assoc adj colored-tree))
					   :last-seen (vertex cur))))
				(if (null next)
				    nil
				    next)))))
	      (if (notany #'null child-colors)
		  (return-from outer (cons (list (vertex cur)
						 (adj-list cur)
						 c)
					   child-colors))))))))

(defun extract-first-tree (forest &key (cur (car forest)) (last-seen nil))
  "assumes car is name, cadr is adj, and caddr is colors. NOTE: THIS DOES NOT CHECK FOR CYCLES -- ONLY PASS IN TREES"
  (if (null (remove last-seen (adj-list cur)))
      (list cur)
      (cons cur
	    (loop for adj in (remove last-seen (adj-list cur))
	       append (extract-first-tree forest
					   :cur (assoc adj forest)
					   :last-seen (car cur))))))



(defun sub-graph-to-assoc (sub-graph graph)
  "Get your adjacencies back!"
  (mapcar #'(lambda (x)
	      (cons x (list (intersection
			     sub-graph
			     (adj-list (assoc x graph))))))
	  sub-graph))


(defun remove-cutset-color (cgraph colored-cutset)
  "constrain non-cutset with cutset's colorings"
  (remove nil
	  (loop for v in cgraph
	     collect (if (member (vertex v)
				 (mapcar #'car colored-cutset))
			 nil
			 (list (vertex v)
			       (set-difference
				(adj-list v)
				(mapcar #'car colored-cutset))
			       (set-difference
				(caddr v)
				(let ((adjs-in-common
				       (intersection
					(mapcar #'car colored-cutset)
					(adj-list v))))
				  (mapcar
				   #'(lambda (y)
				       (caddr
					(assoc y colored-cutset)))
				   adjs-in-common))))))))

(defun mcc (graph colors)
  "Run MGA"
  (let* ((cutset (mga (weight-graph *50-states* colors)))
	 (sorted-cutset (topo-sort cutset)))
    (mapcar #'(lambda (x) (list (car x) (caddr x)))
	    (color2 (mapcar #'(lambda (y)
				(list (vertex y)
				      (adj-list y)
				      colors)) graph)
		    sorted-cutset colors))))

(defun cc (graph colors)
  "Run GA"
  (let* ((cutset (ga (agraph->awgraph graph)))
	 (sorted-cutset (topo-sort
			 (sub-graph-to-assoc cutset graph))))
    (mapcar #'(lambda (x) (list (car x) (caddr x)))
	    (color2 (mapcar #'(lambda (y)
				(list (vertex y)
				      (adj-list y)
				      colors)) graph)
		    sorted-cutset colors)))) 



(defun color2 (cgraph cutset colors)
  "Does the coloring"
  (let* ((colored-cutset (color-cutset (mapcar #'(lambda (y)
						   (append y (list colors)))
					       cutset)))
	 (updated-cgraph (remove-cutset-color cgraph colored-cutset)))
    ;;finish up and combine
    (append colored-cutset (color-forest updated-cgraph))))

(defun color-cutset (cutset)
  "wrapper function"
  (color-cutset2 (cdr cutset) :cur (car cutset)))

(defun color-cutset2 (cutset &key (cur (car cutset)) (seen-nodes nil))
  "recursively backtrack to get a colored cutset"
  (cond
    ((null (caddr cur)) nil)
    ((null cutset) (list (list (vertex cur)
				 (adj-list cur)
				 (caaddr cur))))
    (t
     (loop for c in (caddr cur)
	do (let* ((new-cutset cutset)
		 (new-colors
		  (loop for adj in (set-difference (adj-list cur) seen-nodes)
		     collect (progn
			       (setf new-cutset
				     (cons
				      (remove-color-from cutset adj c)
				      (remove
				       (assoc adj cutset)
				       cutset)))
				(remove c (caddr (assoc adj cutset)))))))
	     (if (some #'null new-colors)
		 nil
		 (let ((cc-ret (color-cutset2 (cdr new-cutset)
					     :cur (car new-cutset)
					     :seen-nodes (cons
							  (vertex cur)
							  seen-nodes))))
		   (if (null cc-ret)
		       nil
		       (return-from color-cutset2 (cons
						  (list
						   (vertex cur)
						   (adj-list cur)
						   c) cc-ret))))))))))



(defun remove-color-from (cgraph cn c)
   (list
    (vertex (assoc cn cgraph))
    (adj-list (assoc cn cgraph))
    (remove c (caddr (assoc cn cgraph)))))


(defun apply-color-cutset-constraints (cgraph colored-cutset)
  (let ((ret-graph (copy-alist cgraph)))
    (loop for ccn in colored-cutset
       do (setf ret-graph
		(mapcar #'(lambda (x)
			    (if (member
				 (vertex ccn)
				 (adj-list x))
				(list (vertex x)
				      (remove (vertex ccn) (adj-list x))
				      (remove (caddr ccn) (caddr x)))
				x))
			ret-graph)))
ret-graph))
