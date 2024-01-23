(defstruct result
  value
  stats
  execution_time
)
(defun result_constructer (node stats)
  (make-result :value (get_value node) :stats stats :execution_time (- (get-internal-real-time) (statistics-init_time stats)))
)
(defun get_value (node)
  (node-value (car (cdr (built_path node))))
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
;o número de nós analisados,o número de cortes efetuados (de cada tipo), o tempo gasto em cada jogada e o tabuleiro atual.
;statistics definition
(defstruct statistics
  init_time
  analised_nodes
  alfa_cuts
  beta_cuts
)
(defun statistics_constructer (stats analised_node alfa_cuts beta_cuts)
  (if (null stats)
    (make-statistics
      :init_time (get-internal-real-time)
      :analised_node analised_node
      :alfa_cuts alfa_cuts
      :beta_cuts beta_cuts
    )
    (make-statistics
      :init_time (statistics-init_time stats)
      :analised_node (+ (statistics-analised_node stats) analised_node)
      :alfa_cuts (+ (statistics-alfa_cuts stats) alfa_cuts)
      :beta_cuts (+ (statistics-beta_cuts stats) beta_cuts)
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

;(defun result_constructer (node stats)
;(defun statistics_constructer (stats analised_node alfa_cuts beta_cuts)
(defun minmax (state depth heuristic expand);max_time
  (recursive_minmax (node_constructer nil state heuristic) t depth heuristic expand)
)
(defun recursive_minmax (node is_maxing depth heuristic expand);max_time
    (let    ((child_nodes (expand_node node expand heuristic))
            )
    
        (cond ((or (= depth 0) (NULL child_nodes)) node)
              (is_maxing
                (calculate_max_eval child_nodes (min_node) depth heuristic expand)
              )
              (t
                (calculate_min_eval child_nodes (max_node) depth heuristic expand)
              )
        )
    )
)
(defun calculate_max_eval (nodes max_eval depth heuristic expand); max_time 
    (let ((node (car nodes)))      
      (cond ((NULL node) max_eval)
          (t 
            (let*  ( (eval (recursive_minmax node nil (- depth 1) heuristic expand))
                    (new_max_eval (get_max eval max_eval))
                  )
              (calculate_max_eval (cdr nodes) new_max_eval depth heuristic expand)
            ) 
          )           
      )
    )
)
(defun calculate_min_eval (nodes min_eval depth heuristic expand); max_time 
  (let ((node (car nodes)))      
    (cond ((NULL node) min_eval)
        (t 
          (let*  ( (eval (recursive_minmax node t (- depth 1) heuristic expand))
                  (new_min_eval (get_min eval min_eval))
                )
            (calculate_min_eval (cdr nodes) new_min_eval depth heuristic expand)
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
(defun get_min (node1 node2)
    (if (< (eval_node node1) (eval_node node2))
        node1
        node2
    )
)
(defun expand_node (node expand heuristic)
  (mapcar #'(lambda (val) (node_constructer node val heuristic)) (apply expand (list (node-value node))))
)





















(defun alfabeta (state play_turn depth max_time heuristic)


)
(defun recursive_alfabeta (node alfa beta is_maxing depth heuristic expand);max_time
    (let    ((child_nodes (expand_node node expand heuristic))
            )
    
        (cond ((or (= depth 0) (NULL child_nodes)) node)
              (is_maxing
                (calculate_max_eval child_nodes alfa beta -1000000 depth heuristic expand)
              )
              (t
                (calculate_min_eval child_nodes alfa beta 1000000 depth heuristic expand)
              )
        )
    )
)

(defun calculate_max_eval (nodes alfa beta max_eval depth heuristic expand); max_time 
    (let*   ( (node (car nodes))
              (eval (recursive_alfabeta node alfa beta nil (- depth 1) heuristic expand))
              (new_max_eval (get_max eval max_eval))
              ;(new_alfa (get_max eval alfa))
            )
            (cond ((NULL node) max_eval)
                  ;((<= beta new_alfa) (calculate_max_eval (cdr nodes) new_alfa beta new_max_eval depth heuristic expand))
                  (t 
                    (calculate_max_eval (cdr nodes) alfa beta new_max_eval depth heuristic expand)
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

(defun calculate_min_eval (nodes alfa beta min_eval depth heuristic expand); max_time 
  (let*   ( (node (car nodes))
            (eval (recursive_alfabeta node alfa beta t (- depth 1) heuristic expand))
            (new_min_eval (get_min eval min_eval))
            ;(new_beta (get_min eval beta))
          )
          (cond ((NULL node) min_eval)
                ;((<= new_beta alfa) ;(calculate_min_eval (cdr nodes) new_alfa beta new_max_eval depth heuristic expand))
                (t 
                  (calculate_min_eval (cdr nodes) alfa beta new_min_eval depth heuristic expand)
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