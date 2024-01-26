(defstruct result
  value
  stats
  execution_time
)
(defun result_constructer (node stats)
  (make-result :value (get_value node) :stats stats :execution_time (- (get-internal-real-time) (statistics-init_time stats)))
)
(defun get_value (node)
  (let ((val (car (cdr (built_path node)))))
    (if (NULL val)
      nil
      (node-value  val)
    )
  )
)
(defun built_path (node)
"Constroi o caminho desde do no folha ate ao no raiz"
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
      :analised_nodes analised_node
      :alfa_cuts alfa_cuts
      :beta_cuts beta_cuts
    )
    (make-statistics
      :init_time (statistics-init_time stats)
      :analised_nodes (+ (statistics-analised_nodes stats) analised_node)
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
(defun node_compare (node1 node2)
"Compara a heuristica do node1 com o node2 se node1 for maior o resultado é negativo se for o node2 maior é positivo se forem iguais é 0"
  (let ((eval1 (eval_node node1)) (eval2 (eval_node node2)))
    (cond ((or (NULL eval1) (NULL eval2)) -1)
          (t (- eval2 eval1))
    )
  )
)
(defun eval_node (node) 
"Retorna a heuristica"
  (if (null node) nil (node-heuristic node))
)
(defun min_node () (make-node :heuristic -1000000))
(defun max_node () (make-node :heuristic 1000000))

(defun get_max (node1 node2)
"Retorna o node com maior heuristica"
    (if (> (eval_node node1) (eval_node node2))
        node1
        node2
    )
)
(defun get_min (node1 node2)
"Retorna o node com menor heuristica"
    (if (< (eval_node node1) (eval_node node2))
        node1
        node2
    )
)
(defun expand_node (node expand heuristic)
"Aplica a função expand para calcular os nos filhos e a função heuristic para calcular o seu valor heuristico e retorna uma lista de nodes"
  (if (NULL node)
    nil
    (mapcar #'(lambda (val) (node_constructer node val heuristic)) (apply expand (list (node-value node))))
  )
)
(defun termination_condition (node depth stats max_time) 
"Verifica se o node é terminal"
  (let ((exec_time (- (get-internal-real-time) (statistics-init_time stats))))
    (if (or (NULL node) (= depth 0) (>= exec_time max_time))
      nil
      t
    )
  )
  
)





(defun alfabeta (state depth heuristic expand max_time)
"Algoritmo alfabeta que usa a função expand para calcular os estados filhos e a função heuristica para calcular o sue valor heuristico"
  (multiple-value-bind 
    (node stats) 
    (recursive_alfabeta 
      (node_constructer nil state heuristic)  
      (min_node) 
      (max_node) 
      t 
      depth 
      heuristic 
      expand 
      (statistics_constructer nil 0 0 0)
      max_time
    ) 
    (result_constructer node stats)
  )
)
(defun recursive_alfabeta (node alfa beta is_maxing depth heuristic expand stats max_time)
    (let    ((child_nodes (expand_node node expand heuristic)))
        (cond ((NULL (termination_condition child_nodes depth stats max_time))
                (values node stats)
              )
              (is_maxing
                (calculate_max_eval_alfabeta child_nodes alfa beta (min_node) depth heuristic expand stats max_time)
              )
              (t
                (calculate_min_eval_alfabeta child_nodes alfa beta (max_node) depth heuristic expand stats max_time)
              )
        )
    )
)
(defun calculate_max_eval_alfabeta (nodes alfa beta max_eval depth heuristic expand stats max_time)
  (let ((node (car nodes)))      
    (cond ((NULL (termination_condition node depth stats max_time)) (values max_eval stats))
      (t 
          (multiple-value-bind 
            (eval new_stats)
            (recursive_alfabeta node alfa beta nil (- depth 1) heuristic expand stats max_time)
            (let  ( (new_max_eval (get_max eval max_eval))
                    (new_alfa (get_max eval alfa))
                  )
              (if (<= (node_compare new_alfa beta) 0)
                (values new_max_eval (statistics_constructer new_stats 1 1 0))
                (calculate_max_eval_alfabeta  (cdr nodes)  new_alfa  beta  new_max_eval  depth  heuristic  expand  (statistics_constructer new_stats 1 0 0)  max_time) 
              )
            )
          )
      )
    )           
  )
)
(defun calculate_min_eval_alfabeta (nodes alfa beta min_eval depth heuristic expand stats max_time)
  (let ((node (car nodes)))      
    (cond ((NULL (termination_condition node depth stats max_time)) (values min_eval stats))
      (t 
          (multiple-value-bind 
            (eval new_stats)
            (recursive_alfabeta node alfa beta t (- depth 1) heuristic expand stats max_time)
            (let  ( (new_min_eval (get_min eval min_eval))
                    (new_beta (get_min eval beta))
                  )
              (if (<= (node_compare alfa new_beta) 0)
                (values new_min_eval (statistics_constructer new_stats 1 0 1))
                (calculate_min_eval_alfabeta (cdr nodes) alfa new_beta new_min_eval depth heuristic expand (statistics_constructer new_stats 1 0 0) max_time) 
              )
            )
          )
      )
    )  
  )
)







(defun minmax (state depth heuristic expand);max_time
  (multiple-value-bind 
    (node stats) 
    (recursive_minmax (node_constructer nil state heuristic) t depth heuristic expand (statistics_constructer nil 0 0 0)) 
    (result_constructer node stats)
  )
)
(defun recursive_minmax (node is_maxing depth heuristic expand stats);max_time
    (let    ((child_nodes (expand_node node expand heuristic)))
        (cond ((or (= depth 0) (NULL child_nodes)) 
                (values node stats)
              )
              (is_maxing
                (calculate_max_eval child_nodes (min_node) depth heuristic expand stats)
              )
              (t
                (calculate_min_eval child_nodes (max_node) depth heuristic expand stats)
              )
        )
    )
)
(defun calculate_max_eval (nodes max_eval depth heuristic expand stats); max_time 
  (let ((node (car nodes)))      
    (cond ((NULL node) (values max_eval stats))
      (t 
          (multiple-value-bind 
            (eval new_stats)
            (recursive_minmax node nil (- depth 1) heuristic expand stats)
            (let ((new_max_eval (get_max eval max_eval)))
              (calculate_max_eval (cdr nodes) new_max_eval depth heuristic expand (statistics_constructer new_stats 1 0 0)) 
            )
          )
      )
    )           
  )
)
(defun calculate_min_eval (nodes min_eval depth heuristic expand stats); max_time 
  (let ((node (car nodes)))      
    (cond ((NULL node) (values min_eval stats))
      (t 
          (multiple-value-bind 
            (eval new_stats)
            (recursive_minmax node t (- depth 1) heuristic expand stats)
            (let ((new_min_eval (get_min eval min_eval)))
              (calculate_min_eval (cdr nodes) new_min_eval depth heuristic expand (statistics_constructer new_stats 1 0 0)) 
            )
          )
      )
    )  
  )
)



(defun test_alfabeta_equal_to_minmax ()
  
  (loop for i from 0 to 10 do
    (let ((state (example_state)))
      (format t 
"
*******************************
Minmax    - ~a
Alfabeta  - ~a
*******************************
"
        (player-position (get_player (result-value (minmax state 2 #'h1_player1 #'get_all_possible_plays)) -1))
        (player-position (get_player (result-value (alfabeta state 2 #'h1_player1 #'get_all_possible_plays 5000)) -1))
      )
    )
  )
)