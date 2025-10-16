;;;; ============================================================
;;;; Exercício 15 — Menu de Produtos / Compras (Common Lisp)
;;;; ============================================================

(defpackage :mercado
  (:use :cl)
  (:export :main))

(in-package :mercado)

;;;; ------------------------ UTIL ------------------------------

(defun clear-screen ()
  "Limpa a tela de forma simples (imprime várias quebras de linha)."
  (dotimes (i 40) (terpri)))

(defun read-trimmed-line (&optional (prompt nil))
  (when prompt (format t "~A" prompt))
  (let ((s (read-line *standard-input* nil "")))
    (string-trim '(#\Space #\Tab) s)))

(defun wait-key (&optional (msg "~%-- pressione Enter para continuar --"))
  (format t "~A" msg)
  (finish-output)
  (read-line))

(defun parse-positive-integer (str &optional (default nil))
  (handler-case
      (let ((n (parse-integer str :junk-allowed nil)))
        (if (plusp n) n default))
    (error () default)))

(defun money (x)
  "Formata número com 2 casas decimais."
  (format nil "~,2F" x))

;;;; ------------------------ DADOS ------------------------------

;; Tabela: chave = nome em minúsculas; valor = (display-name . price)
(defparameter *produtos* (make-hash-table :test 'equal))

(defun add-produto (nome preco)
  (setf (gethash (string-downcase nome) *produtos*)
        (cons nome preco)))

;; Semente de produtos (exemplos; adicione mais se quiser)
(mapc (lambda (p) (apply #'add-produto p))
      '(("Arroz 5kg"               23.90)
        ("Feijão Preto 1kg"        8.49)
        ("Macarrão Penne 500g"     5.79)
        ("Azeite de Oliva 500ml"   29.90)
        ("Leite 1L"                4.79)
        ("Ovos Dúzia"              12.50)
        ("Açúcar 1kg"              4.29)
        ("Café 500g"               18.90)
        ("Farinha de Trigo 1kg"    6.19)
        ("Molho de Tomate 340g"    3.99)
        ("Sal 1kg"                 2.49)
        ("Manteiga 200g"           12.90)
        ("Queijo Mussarela 200g"   13.50)
        ("Presunto 200g"           9.90)
        ("Peito de Frango 1kg"     17.90)
        ("Carne Moída 1kg"         29.90)
        ("Banana 1kg"              6.99)
        ("Maçã 1kg"                8.99)
        ("Tomate 1kg"              7.49)
        ("Cebola 1kg"              4.99)
        ("Alho 200g"               7.90)
        ("Batata 1kg"              6.49)
        ("Pão de Forma"            9.90)
        ("Iogurte 170g"            3.49)
        ("Granola 1kg"             24.90)
        ("Atum Lata 170g"          7.99)
        ("Sardinha Lata 125g"      5.49)
        ("Água Mineral 1,5L"       3.29)
        ("Refrigerante 2L"         9.49)
        ("Suco Uva 1L"             10.90)
        ("Chocolate 90g"           6.99)
        ("Biscoito Recheado 130g"  3.59)
        ("Sabonete"                2.99)
        ("Detergente 500ml"        2.89)
        ("Sabão em Pó 800g"        12.90)
        ("Amaciante 1L"            11.50)
        ("Papel Higiênico 12un"    18.90)
        ("Shampoo 300ml"           15.90)
        ("Condicionador 300ml"     16.90)
        ("Escova de Dentes"        7.50)
        ("Creme Dental"            5.90)
        ("Esponja de Aço"          2.50)
        ("Esponja Multiuso"        2.20)
        ("Desinfetante 1L"         8.90)
        ("Álcool 70% 1L"           9.90)
        ("Guardanapo 50un"         5.20)
        ("Filtro de Café 30un"     4.80)
        ("Fermento Químico 100g"   3.90)
        ("Leite Condensado 395g"   6.99)
        ("Creme de Leite 200g"     4.99)
        ("Milho Verde Lata"        4.79)
        ("Ervilha Lata"            4.79)
        ("Azeitona 100g"           6.90)
        ("Ketchup 400g"            8.50)
        ("Mostarda 200g"           6.50)
        ("Maionese 500g"           11.90)
        ("Granulado 150g"          4.70)))

(defun lista-produtos ()
  "Retorna lista de (nome . preço) em ordem alfabética por nome."
  (let (acc)
    (maphash (lambda (_k v) (push v acc)) *produtos*)
    (sort acc #'string-lessp :key #'car)))

(defun buscar-produto (nome)
  "Procura por NOME (case-insensitive). Retorna (display-name . price) ou NIL."
  (gethash (string-downcase nome) *produtos*))

;;;; ------------------------ (a) PESQUISAR ----------------------

(defun menu-pesquisar-preco ()
  (clear-screen)
  (format t "=== Pesquisar preço de um produto ===~%")
  (let* ((nome (read-trimmed-line "Digite o nome do produto: "))
         (res (buscar-produto nome)))
    (cond
      (res (format t "~%~A custa R$ ~A.~%"
                   (car res) (money (cdr res))))
      (t   (format t "~%Produto não encontrado.~%"))))
  (wait-key))

;;;; ------------------------ (b) LISTAR -------------------------

(defun imprimir-linha-produto (idx nome preco)
  (format t "~3D. ~-38A  R$ ~8A~%"
          idx nome (money preco)))

(defun menu-listar-produtos ()
  (clear-screen)
  (format t "=== Lista de produtos (ordem alfabética) ===~%~%")
  (let ((itens (lista-produtos))
        (count 0))
    (dolist (p itens)
      (incf count)
      (imprimir-linha-produto count (car p) (cdr p))
      (when (and (> count 0) (zerop (mod count 20)))
        (wait-key)
        (clear-screen)
        (format t "=== Lista de produtos (continuação) ===~%~%"))))
  (format t "~%--- fim da lista ---~%")
  (wait-key))

;;;; ------------------------ (c) COMPRAS ------------------------

(defun adicionar-ao-carrinho (carrinho nome qtd preco)
  "CARRINHO é lista de itens: ((nome qtd preco) ...). Retorna novo carrinho."
  (cons (list nome qtd preco) carrinho))

(defun imprimir-recibo (carrinho)
  (clear-screen)
  (format t "=========== RECIBO ===========~%")
  (format t "~A~%" (make-string 34 :initial-element #\-))
  ;; corrigido — 4 colunas → 4 placeholders
  (format t "~A~%" (format nil "~A~15T~A~27T~A~40T~A"
                           "Produto" "Qtd" "Preço" "Subtotal"))
  (format t "~A~%~%" (make-string 50 :initial-element #\-))
  (let ((total 0.0)
        (itens (nreverse carrinho)))
    (dolist (item itens)
      (destructuring-bind (nome qtd preco) item
        (let ((subtotal (* qtd preco)))
          (incf total subtotal)
          (format t "~A~15T~D~27T~A~40T~A~%"
                  nome qtd (money preco) (money subtotal)))))
    (format t "~%~A~%" (make-string 50 :initial-element #\-))
    (format t "TOTAL FINAL: R$ ~A~%~%" (money total))
    (wait-key)))

(defun menu-fazer-compras ()
  (clear-screen)
  (format t "=== Fazer compras ===~%")
  (format t "Digite itens no formato: nome do produto / quantidade~%")
  (format t "Ex.: Cafe 500g / 2~%")
  (format t "Digite somente Enter para encerrar.~%~%")
  (let ((carrinho '()))
    (loop
      (let* ((linha   (read-trimmed-line "> "))
             (vazia?  (string= linha ""))
             (fim?    (string-equal linha "fim"))
             (pos     (position #\/ linha))
             (nome    (string-trim '(#\Space #\Tab)
                                   (if pos (subseq linha 0 pos) linha)))
             (qtd-str (when pos
                        (string-trim '(#\Space #\Tab)
                                     (subseq linha (1+ pos)))))
             (qtd     (or (and qtd-str (parse-positive-integer qtd-str)) 1))
             (res     (and (not vazia?) (not fim?) (buscar-produto nome))))
        (when (or vazia? fim?) (return))
        (cond
          ((null res)
           (format t "Produto \"~A\" não encontrado. Tente novamente.~%" nome))
          ((not qtd)
           (format t "Quantidade inválida. Use número inteiro positivo.~%"))
          (t
           (push (list (car res) qtd (cdr res)) carrinho)
           (format t "~A x~D adicionado(s). Subtotal: R$ ~A~%"
                   (car res) qtd (money (* qtd (cdr res))))))))
    (if carrinho
        (imprimir-recibo carrinho)
        (progn
          (format t "~%Nenhum item no carrinho.~%")
          (wait-key)))))

;;;; ------------------------ MENU PRINCIPAL ---------------------

(defun mostrar-menu ()
  (clear-screen)
  (format t "=======================================~%")
  (format t "       SISTEMA DE PRODUTOS / COMPRAS    ~%")
  (format t "=======================================~%~%")
  (format t "1) Pesquisar preço de um produto~%")
  (format t "2) Listar produtos (ordem alfabética)~%")
  (format t "3) Fazer compras~%")
  (format t "0) Sair~%~%"))

(defun main ()
  (loop
    (mostrar-menu)
    (let* ((op (read-trimmed-line "Escolha uma opção: ")))
      (case (and (> (length op) 0) (char op 0))
        (#\1 (menu-pesquisar-preco))
        (#\2 (menu-listar-produtos))
        (#\3 (menu-fazer-compras))
        (#\0 (return (format t "~%Até mais!~%")))
        (t   (format t "~%Opção inválida.~%")
             (wait-key))))))

#+sbcl
(when (find-package :mercado)
  (mercado:main))
