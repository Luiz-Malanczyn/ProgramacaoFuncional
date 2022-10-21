;; Luiz Eduardo Malanczyn de Oliveira 

;; 1) 
(defn ultimo [l] (reduce (fn [a b] b) l))


(println (format "Função ultimo: Valor: %s; Resultado: %s"  "[1 5 10]" (str (ultimo [1 5 10]))))

;; 2)   
(defn penultimo [l] (reduce (fn [a b] b) (pop l)))

(println (format "Função penultimo: Valor: %s; Resultado: %s" "[1 5 10]" (str (penultimo [1 5 10]))))

;; 3) 
(defn elementoN [lista n] (loop [counter n lista2 lista] (if (zero? counter) (first lista2) (recur (dec counter) (rest lista2)))))

(println (format "Função elementoN: Valor: %s; Resultado: %s"  "[1 5 10], 1" (str (elementoN [1 5 10] 1))))

;; 4) 
(defn inverso [l] (reduce conj '() l))

(println (format "Função inverso: Valor: %s; Resultado: %s"  "[1 5 10]" (str (inverso [1 5 10]))))

;; 5) 

(defn mdc [a b] (if (zero? b) a (recur b (mod a b))))

(println (format "Função mdc: Valor: %s; Resultado: %s"  "13 22" (str (mdc 13 22))))
