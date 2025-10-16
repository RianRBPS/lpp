;; ============================================================
;; Exercício 17 – Macros: Fatorial e Números Primos (Clojure)
;; MODO SCRIPT (sem namespace) — chama -main automaticamente
;; ============================================================

;; --------- util: raiz inteira (floor) ----------
(defn int-sqrt
  "Retorna ⌊sqrt(n)⌋ como inteiro."
  ^long [^long n]
  (long (Math/floor (Math/sqrt (double n)))))

;; --------- primo? ----------
(defn primo?
  "Retorna true se n for primo, false caso contrário."
  [n]
  (cond
    (<= n 1) false
    (= n 2)  true
    (even? n) false
    :else
    (let [lim (int-sqrt n)]
      (not-any?
        #(zero? (mod n %))
        (cons 2 (range 3 (inc lim) 2))))))

;; --------- fatorial ----------
(defn fatorial
  "Fatorial de n, usando *' para BigInt (evita overflow)."
  [n]
  (reduce *' (range 1 (inc n))))

;; --------- macro with-prime-factorial ----------
(defmacro with-prime-factorial
  "Imprime os primos até n, imprime fatorial(n) e retorna o fatorial.
   Uso: (with-prime-factorial 10)"
  [n]
  `(do
     (print "Primos até" ~n ": ")
     ;; usar gensym (i#) evita símbolo qualificado em binding
     (doseq [i# (range 2 (inc ~n))]
       (when (primo? i#)
         (print i# " ")))
     (println)
     (let [res# (fatorial ~n)]
       (println "Fatorial de" ~n "=" res#)
       res#)))

;; --------- main (execução direta) ----------
(defn -main
  "Roda um exemplo com n=10, ou lê n da linha de comando."
  [& args]
  (let [n (if (seq args)
            (Long/parseLong (first args))
            10)]
    (println (with-prime-factorial n))))

;; --- EXECUÇÃO COMO SCRIPT (chama -main automaticamente) ---
(apply -main *command-line-args*)
