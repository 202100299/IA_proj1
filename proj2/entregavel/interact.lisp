(load "jogo.lisp")
(load "algoritmo.lisp")

(defun play ()
"Função que lança o programa"
 (main_menu)
)

(defun jogar (estado tempo)
  (let* ( (state (result-value (computer_move (verify_state estado) tempo)))
          (board (get_state_board_points state))
          (player1 (get_player state -1))
          (player2 (get_player state 1))
          (new_board (set_board_value (set_board_value board (player-position player1) -1) (player-position player2) -2))
      )
    new_board
  )
)
(defun verify_state (board)
  (if (typep board 'state) 
    board 
    (let*  ( (player1_pos (seach_board board -1))
            (player2_pos (seach_board board -2))
            (state (result-value (computer_move (verify_state estado) tempo)))
            (new_board (set_board_value (set_board_value board player1_pos nil) player2_pos nil))
          )
      (state_constructer_with_params new_board (player_constructer 0 player1_pos '()) (player_constructer 0 player2_pos '()) -1)
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



(defun print_state (state) 
"Mostra o estado na terminal"
  (format t "~a" (format_state_to_string state))
)
(defun format_state_to_string (state)
"Converte o estado em uma string"
(if (NULL state) (format nil "Nenhuma jogada executada!")
(let* ( (board (get_state_board_points state))
        (player1 (get_player state -1))
        (player2 (get_player state 1))
        (new_board (set_board_value (set_board_value board (player-position player1) -1) (player-position player2) -2))
      )
    
    (print_board new_board)
    (format nil
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
))
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
(defun board_to_string (board)
  (format nil 
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
(defun print_board (board)
"Mostra no terminal uma lista de listas de 10*10 em forma de tabela"
  (format t "~a" (board_to_string board))
)
(defun format_line (list)
"Formata uma lista"
  (format nil "| ~{~<~a~>~^ ~}" (mapcar (lambda (n) (format nil "~v,' d |" 3 n)) list))
)







(defun position_to_string (pos)
"Transforma a posição em string"
  (let ((x (horse_position-x pos)) (y (horse_position-y pos)))
    (cond ((or (< x 0) (< y 0)) (format nil "NN"))
          (T (format nil "~a~a" (nth x '(A B C D E F G H I J)) y))
    )
  )
  
)
(defun string_to_position (str)
"Converte uma string em posição"
  (cond ((not (= (length str) 2)) nil) 
        (t (let ( (x (- (char-code (char str 0)) 97)) 
                  (y (digit-char-p (char str 1)))
                )
            (cond ((not (and (not (null y)) (>= x 0) (<= x 9) (>= y 0) (<= y 9))) (position_constructer -10 -10))
                  (t (position_constructer x y))
            )
        ))
  )
)









;(defun get_path () 
;  (format nil "C:\\Users\\ruies\\Documents\\programacao\\3ano\\IA\\proj2\\entregavel\\log.dat")
;)
(defun get_path () 
"Pede ao utilizador o caminho para o ficheiro log.dat"
  (show_question "Qual o caminho do ficheiro log.dat?")
)

(defun main_menu ()
"Menu principal"
  (let ((choice (show_question (format nil"Bem-vindo ao Jogo de Xadrez!~%1. Computador contra o Computador~%2. Jogador contra o Computador~%3. Sair~%"))))
    (case choice
      (1 (computer_vs_computer_menu))
      (2 (player_vs_computer_menu))
      (3 (format t "Ate logo!~%"))
      (t (format t "Escolha invalida. Tente novamente.~%")(main_menu)))))



(defun computer_vs_computer_menu ()
"Menu PC vs PC"
  (let ((max_time (show_question (format nil "Voce escolheu Computador contra Computador.~%Por favor, insira o tempo maximo para cada jogada(milisegundos): "))))
      (progn 
        (format t "Tempo maximo escolhido: ~a milisegundos.~%" max_time)
        (game_round (example_state) max_time 0 (get_path))
      )
    ))



(defun player_vs_computer_menu ()
"Menu Player vs PC"
  (let  ( (max_time (show_question (format nil "Voce escolheu jogar contra o Computador.~%Por favor, insira o tempo maximo para cada jogada do computador(milisegundos): ")))
          (player_color (show_question (format nil "Voce sera o Cavalo (P) preto ou branco (B)?")))
        )
    (cond ((string= player_color "P") 
            (progn 
              (format t "Você escolheu ser o Cavalo Preto.~%")
              (game_round (example_state) max_time 1 (get_path))            
            )
          )
          ((string= player_color "B")
            (progn 
              (format t "Você escolheu ser o Cavalo Branco.~%")
              (game_round (example_state) max_time -1 (get_path))            
            )
          )
          (t
            (progn 
              (format t "Escolha invalida. Tente novamente.~%") 
              (player_vs_computer_menu)
            )
          )
    )
  )
)




(defun format_statistics_to_string (result)
"Converte o resultado em string"
  (format nil "
~a
Estatisticas
Tempo de Execucao: ~a milisegundos
Nos Analisados: ~a
Cortes Alfa: ~a
Cortes Beta: ~a

"
          (format_state_to_string (result-value result))
          (result-execution_time result)
          (statistics-analised_nodes (result-stats result))
          (statistics-alfa_cuts (result-stats result))
          (statistics-beta_cuts (result-stats result))
  )
)



(defun print_result (result path)
"Mostra na terminal e acrescenta num ficheiro o resultado  "
  (let ((str (format_statistics_to_string result)))
    (progn
      (format t str)
      (append_to_file path str)
    )
  )
)



(defun append_to_file (file-path text)
  "Adiciona o texto ao final do arquivo"
  (with-open-file (stream file-path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (format stream "~a~%" text)
  )
)








(defun player_move (state)  
"Pede ao utilizador a proxima jogada"
  (if (NULL (is_first_move state))
    (move_piece state (get_player_move state))
    (progn (print_state state) (first_play state (get_initial_move)))
  )
)
  

(defun get_initial_move ()
"Pede ao utilizador qual a primeira jogada "
  (let*  ((option (string-downcase (show_question "Qual e a sua opcao")))
          (init_x (- (char-code (char option 0)) 97)) 
        )
    (if (or (< init_x 0) (> init_x 9))
      nil
      init_x
    )
  )
)
(defun get_player_move (state)
"Pede ao utilizador a proxima jogada"
  (let*  ((player (get_player state (get_play_turn state)))
          (current_position (player-position player))
          (option (string-downcase (show_question "Qual e a sua opcao")))
          (new_vector (calculate_vector current_position (string_to_position option)))
        )

    (cond ((NULL (get_all_possible_plays state)) 
            (format t "Sem jogadas possiveis")
            '(0 0)
          )
          ((NULL (check_player_move new_vector)) 
            (progn 
              (format t "Jogada nao possivel") 
              (get_player_move state)
            )
          )
          (t  new_vector)
      
    )
  )
)

(defun game_round (state max_time player_turn path) 
"Ronda de jogo"
  (cond ((NULL state) nil)
        ((and (NULL (get_all_possible_plays state)) (NULL (get_all_possible_plays (switch_play_turn state)))) 
          (format t "O jogo terminou!")
        )
        (t
          (let  ( (result (if (= player_turn (state-play_turn state))
                            (player_move state)
                            (computer_move state max_time)
                          )
                  )
                )
              (cond ((NULL result)
                      (game_round 
                            (switch_play_turn state)
                            max_time
                            player_turn
                            path                      
                      )
                    )
                    ((= player_turn (state-play_turn state))
                      (game_round 
                          result
                          max_time
                          player_turn
                          path                      
                      )
                    )
                    ((NULL (result-value result))
                      (progn
                        (let ((next_state (switch_play_turn state)))
                          (print_result result path)
                          (print_state next_state)
                          (game_round 
                            next_state
                            max_time
                            player_turn
                            path                      
                          ))
                      )
                    )   
                    (t 
                      (progn
                        (print_result result path)
                        (game_round 
                          (result-value result)
                          max_time
                          player_turn
                          path                      
                        )
                      )
                    )
                )
          )
        )
  )
)

