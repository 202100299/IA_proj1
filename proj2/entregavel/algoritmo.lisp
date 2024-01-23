(defstruct result
  path
  stats
)
(defun result_constructer (node stats)
  (make-result :path (built_path node) :stats stats)
)
(defun built_path (node)
  (cond ((null node)
          nil
        )
        ((null (node-parent_node node))
          (list node)
        )
        (t
          (append (built_path (node-parent_node node)) (list node))
        )
  )
)

;statistics definition
(defstruct statistics
  expanded_nodes
  generated_nodes
)
(defun statistics_constructer (stats expanded_increment generated_increment)
  (if (null stats)
    (make-statistics
      :expanded_nodes expanded_increment
      :generated_nodes generated_increment
    )
    (make-statistics
      :expanded_nodes (+ (statistics-expanded_nodes stats) expanded_increment)
      :generated_nodes (+ (statistics-generated_nodes stats) generated_increment)
    )
  )
)


;;Node definition and constructer
(defstruct node 
  (parent_node NIL)
  (value NIL)
  (heuristic)
)
(defun node_constructer (parent_node value heuristic)
"Construtor de estrutura de dados Node"
  (cond 
    ((NULL value) NIL)
    (T
      (make-node
        :parent_node parent_node
        :value value
        :heuristic (apply heuristic (list value))
      )
    )
  )
)
(defun eval_node (node) 
"Retorna o numero de pontos obtidos no estado"
  (if (null node) nil (node-heuristic node))
)
(defun min_node () (make-node :heuristic -1000000))
(defun max_node () (make-node :heuristic 1000000))


(defun minmax (state depth heuristic expand);max_time
  (recursive_minmax (node_constructer nil state heuristic) t depth heuristic expand)
)
(defun recursive_minmax (node is_maxing depth heuristic expand);max_time
    (let    ((child_nodes (expand_node node expand heuristic))
            )
    
        (cond ((or (= depth 0) (NULL child_nodes)) node)
              (is_maxing
                (claculate_max_eval child_nodes (min_node) depth heuristic expand)
              )
              (t
                (claculate_min_eval child_nodes (max_node) depth heuristic expand)
              )
        )
    )
)

(defun claculate_max_eval (nodes max_eval depth heuristic expand); max_time 
    (let ((node (car nodes)))      
      (cond ((NULL node) max_eval)
          (t 
            (let*  ( (eval (recursive_minmax node nil (- depth 1) heuristic expand))
                    (new_max_eval (get_max eval max_eval))
                  )
              (claculate_max_eval (cdr nodes) new_max_eval depth heuristic expand)
            ) 
          )           
      )
    )
)
(defun get_max (node1 node2)
    (if (> (eval_node node1) (eval_node node2))
        node1
        node2
    )
)

(defun claculate_min_eval (nodes min_eval depth heuristic expand); max_time 
  (let ((node (car nodes)))      
    (cond ((NULL node) min_eval)
        (t 
          (let*  ( (eval (recursive_minmax node t (- depth 1) heuristic expand))
                  (new_min_eval (get_min eval min_eval))
                )
            (claculate_min_eval (cdr nodes) new_min_eval depth heuristic expand)
          ) 
        )           
    )
  )
)
(defun get_min (node1 node2)
    (if (< (eval_node node1) (eval_node node2))
        node1
        node2
    )
)

(defun expand_node (node expand heuristic)
    (if (NULL node) 
      (print "AQUI")
      (mapcar #'(lambda (val) (node_constructer node val heuristic)) (apply expand (list (node-value node))))
    )
    
)





















(defun alfabeta (state play_turn depth max_time heuristic)


)
(defun recursive_alfabeta (node alfa beta is_maxing depth heuristic expand);max_time
    (let    ((child_nodes (expand_node node expand heuristic))
            )
    
        (cond ((or (= depth 0) (NULL child_nodes)) node)
              (is_maxing
                (claculate_max_eval child_nodes alfa beta -1000000 depth heuristic expand)
              )
              (t
                (claculate_min_eval child_nodes alfa beta 1000000 depth heuristic expand)
              )
        )
    )
)

(defun claculate_max_eval (nodes alfa beta max_eval depth heuristic expand); max_time 
    (let*   ( (node (car nodes))
              (eval (recursive_alfabeta node alfa beta nil (- depth 1) heuristic expand))
              (new_max_eval (get_max eval max_eval))
              ;(new_alfa (get_max eval alfa))
            )
            (cond ((NULL node) max_eval)
                  ;((<= beta new_alfa) (claculate_max_eval (cdr nodes) new_alfa beta new_max_eval depth heuristic expand))
                  (t 
                    (claculate_max_eval (cdr nodes) alfa beta new_max_eval depth heuristic expand)
                  )            
            )
    )
)
(defun get_max (node1 node2)
    (if (> (node-heuristic node1) (node-heuristic node1))
        node1
        node2
    )
)

(defun claculate_min_eval (nodes alfa beta min_eval depth heuristic expand); max_time 
  (let*   ( (node (car nodes))
            (eval (recursive_alfabeta node alfa beta t (- depth 1) heuristic expand))
            (new_min_eval (get_min eval min_eval))
            ;(new_beta (get_min eval beta))
          )
          (cond ((NULL node) min_eval)
                ;((<= new_beta alfa) ;(claculate_min_eval (cdr nodes) new_alfa beta new_max_eval depth heuristic expand))
                (t 
                  (claculate_min_eval (cdr nodes) alfa beta new_min_eval depth heuristic expand)
                )            
          )
  )
)
(defun get_min (node1 node2)
    (if (< (node-heuristic node1) (node-heuristic node1))
        node1
        node2
    )
)

(defun expand_node (node expand heuristic)
    (mapcar #'(lambda (val) (node_constructer node val heuristic)) (apply  #'expand (list (node-value node))))
)