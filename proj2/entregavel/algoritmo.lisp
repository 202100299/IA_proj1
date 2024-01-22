;;Node definition and constructer
(defstruct node 
  (parent_node NIL)
  (state)
  (heuristic)
)
(defun node_constructer (parent_node state cost heuristic goal)
"Construtor de estrutura de dados Node"
  (cond 
    ((NULL state) NIL)
    ((NULL heuristic) 
      (make-node
        :parent_node parent_node
        :state state
        :cost cost
        :heuristic nil
      )
    )
    (T
      (make-node
        :parent_node parent_node
        :state state
        :cost cost
        :heuristic (apply heuristic (list state goal))
      )
    )
  )
)
(defun get_node_points (node) 
"Retorna o numero de pontos obtidos no estado"
  (if (null node) 0 (state-points (node-state node)))
)
(defun get_node_cost (node)
"Retorna o custo do Node"
  (cond ((null node) nil)
        ((null (node-heuristic node)) (node-cost node))
        (T (+ (node-heuristic node) (node-cost node)))
  )
)









(defun alfabeta (state play_turn depth max_time heuristic)


)

(defun recursive_alfabeta state nil nil t depth max_time heuristic)