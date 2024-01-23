(defun play ()
  (menu (state_constructer (ramdom_board)))
)
(defun menu (state)
  (game_round (first_play (first_play state (get_initial_move)) (get_initial_move)))
)
(defun game_round (state)
  (progn 
    (print_state state)
    (let* ( (player (get_player state (get_play_turn state)))
            (move_vec (get_player_move (player-position player)))
          )
      (game_round (move_piece state move_vec))
    )
  )
  
)
(defun get_initial_move ()
  (let*  ((option (string-downcase (show_question "Qual e a sua opcao")))
          (init_x (- (char-code (char option 0)) 97)) 
        )
    (if (or (< init_x 0) (> init_x 9))
      nil
      init_x
    )
  )
)
(defun get_player_move (current_position)
  (let*  ((option (string-downcase (show_question "Qual e a sua opcao")))
          (new_vector (calculate_vector current_position (string_to_position option)))
        )

    (cond ((NULL (check_player_move new_vector)) nil)
          (t new_vector)
    )
  )
)




(defun show_question (question)
"Mostra na terminal uma pergunta e retorna a resposta do utilizador"
  (format t "~%> ~a" question)
  (read_option)
)
(defun read_option ()
  "Le o input do utilizador."
  (format t "~%>> ")
  (finish-output)
  (let ((opcao (read))) (clear-input) opcao)
)
;(defun read_option ()
;  "Le o input do utilizador."
;  (format t "~%>> ")
;  (finish-output)
;  (let ((opcao (string-downcase (princ-to-string (read))))) (clear-input) opcao)
;)


(defun print_state (state)
(let* ( (board (get_state_board_points state))
        (player1 (get_player state -1))
        (player2 (get_player state 1))
        (new_board (set_board_value (set_board_value board (player-position player1) -1) (player-position player2) -2))
      )
    
    (print_board new_board)
    (format t 
"
Cavalo ~a a jogar


Player- 1 
  Caminho - Inicio ~a
  Pontos  - ~a pts

Player- 2
  Caminho - Inicio ~a
  Pontos  - ~a pts
" 
      (get_color (get_play_turn state))
      (path_to_string (player-path player1))
      (player-points player1)
      (path_to_string (player-path player2))
      (player-points player2)
    )
)
)
(defun get_color (play_turn) 
  (if (= play_turn -1)
    (format nil "Branco")
    (format nil "Preto")
  )
)
(defun path_to_string (path)
  (format nil "~{~<-> ~a~>~^ ~}" (mapcar #'position_to_string path))
)
;;Print Boards
(defun print_board (board)
"Mostra no terminal uma lista de listas de 10*10 em forma de tabela"
  (format t 
  "
         A     B     C     D     E     F     G     H     I     J
  0   ~a
  1   ~a
  2   ~a
  3   ~a
  4   ~a
  5   ~a
  6   ~a
  7   ~a
  8   ~a
  9   ~a
  " 
  (format_line (nth 0 board))
  (format_line (nth 1 board))
  (format_line (nth 2 board))
  (format_line (nth 3 board))
  (format_line (nth 4 board))
  (format_line (nth 5 board))
  (format_line (nth 6 board))
  (format_line (nth 7 board))
  (format_line (nth 8 board))
  (format_line (nth 9 board))
  )
)
(defun format_line (list)
"Formata uma lista"
  (format nil "| ~{~<~a~>~^ ~}" (mapcar (lambda (n) (format nil "~v,' d |" 3 n)) list))
)







(defun position_to_string (pos)
  (let ((x (horse_position-x pos)) (y (horse_position-y pos)))
    (cond ((or (< x 0) (< y 0)) (format nil "NN"))
          (T (format nil "~a~a" (nth x '(A B C D E F G H I J)) y))
    )
  )
  
)
(defun string_to_position (str)
  (cond ((not (= (length str) 2)) nil) 
        (t (let ( (x (- (char-code (char str 0)) 97)) 
                  (y (digit-char-p (char str 1)))
                )
            (cond ((not (and (not (null y)) (>= x 0) (<= x 9) (>= y 0) (<= y 9))) nil)
                  (t (position_constructer x y))
            )
        ))
  )
)