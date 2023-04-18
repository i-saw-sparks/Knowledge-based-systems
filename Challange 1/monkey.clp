(deftemplate monkey
    (slot location)
    (slot has-banana)
)

(deftemplate table
    (slot location)
)

(deftemplate banana
    (slot location)
)

(defrule monkey-can-reach-table
    ?m <- (monkey (location door) (has-banana no))
    ?t <- (table (location corner))
    =>
    (modify ?m (location center))
    (modify ?t (location center))
    (printout t "Monkey moved the table!" crlf)
)

(defrule monkey-climbs-on-table
    ?m <- (monkey (location center) (has-banana no))
    ?t <- (table (location center))
    =>
    (modify ?m (location on_table))
    (printout t "Monkey climbed on the table." crlf)
)

(defrule monkey-can-reach-banana
    ?m <- (monkey (location on_table) (has-banana no))
    ?b <- (banana (location ceiling))
    ?t <- (table (location center))
    =>
    (modify ?m (has-banana yes))
    (modify ?b (location monkey_hands))
    (printout t "Monkey got the banana!" crlf)
)

(defrule goal-reached
    (monkey (has-banana yes))
    =>
    (printout t "The monkey reached its goal!" crlf)
)

(assert (monkey (location door) (has-banana no)))
(assert (table (location corner)))
(assert (banana (location ceiling)))

(facts)
(run)

(clear)
