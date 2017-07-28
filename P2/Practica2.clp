
;Hechos que utiliza el sistema:

(deffacts HECHOS
  (noDescartado N1H1)
  (noDescartado GRIPE)
  (noDescartado MENINGITIS)
  (noDescartado DENGUE)

  (sin-enf (sintoma CUELLO))
  (sin-enf (sintoma TOS))
  (sin-enf (sintoma RONCHAS))
  (sin-enf  (sintoma FIEBRE))
  (sin-enf  (sintoma MAL))
  (sin-enf  (sintoma DOLOR))

)

;Función para realizar preguntas con opciones:

(deffunction ask-question (?qBEG ?qMID ?qEND $?allowed-values)
  (printout t ?qBEG crlf crlf)
  (printout t ?qMID crlf)
  (printout t "Introduce (" ?qEND"): ")
  (bind ?answer (read))
  (if (lexemep ?answer)
	then (bind ?answer (lowcase ?answer))
  )
  (while (not (member ?answer ?allowed-values)) do
	(printout t ?qBEG crlf crlf)
    	(printout t ?qMID crlf)
    	(printout t "Introduce (" ?qEND"): ")
  	(bind ?answer (read))
  	(if (lexemep ?answer)
  		then (bind ?answer (lowcase ?answer))
  	)
   )
   ?answer)

; Template para gestionar los sintomas con sus caracteristicas
(deftemplate sin-enf
  (slot sintoma)
  (slot intensidad)
  (slot duracion)
)


; Reglas implementadas:

(defrule firstQuestion
  ?x <- (initial-fact)
	=>
  (retract ?x)
	(bind ?r (ask-question
    "¿Cuales son tus síntomas?"
    "Fiebre, Dolor de cabeza, mal estar general"
    "fiebre, dolor, mal, p"
    fiebre dolor mal p))
  (if (neq ?r p)
		then (assert (initial-fact))
         (assert (sin-enf (sintoma ?r)))
  else
    (assert (modulo-hipotesis))
    (retract ?x)
  )
  (watch facts)
)
 ;; MODULO Hipotesis

(defrule hip1
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma FIEBRE))
  (noDescartado GRIPE)
=>
  (assert (hipotesis GRIPE))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip2
  ?ml <- (modulo-hipotesis)
  (noDescartado MENINGITIS)
  (sin-enf (sintoma CUELLO))
  =>
  (assert (hipotesis MENINGITIS))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip3
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma RONCHAS))
  (noDescartado DENGUE)
  =>
  (assert (hipotesis DENGUE))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip4
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma ?x) (duracion SEMANAS))
  (noDescartado N1H1)
  =>
  (assert (hipotesis N1H1))
  (assert (modulo-dd))
  (retract ?ml)
)

;; MODULO descartar hipotesis inicial

(defrule dd1
  ?ml <- (modulo-dd)
  (noDescartado MENINGITIS)
=>
  (assert (dd MENINGITIS))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd2
  ?ml <- (modulo-dd)
  (hipotesis N1H1)
=>
  (assert (dd DENGUE))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd3
  ?ml <- (modulo-dd)
  (hipotesis DENGUE)
=>
  (assert (dd N1H1))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd4
  ?ml <- (modulo-dd)
  (hipotesis MENINGITIS)
=>
  (assert (dd MENINGITIS))
  (assert (modulo-pregunta))
  (retract ?ml)
)
 ;; MODULO Preguntas

(defrule pregunta1
  ?ml <- (modulo-pregunta)
  ?dd <- (dd MENINGITIS)
  ?e1 <- (noDescartado MENINGITIS)
   ?e <- (noDescartado GRIPE)
   =>
  (bind ?q (ask-question
    "¿Rigidez en el cuello o dolor en la nuca?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q si)
      then
        (retract ?e)
      else
        (retract ?e1)
    )

    (retract ?ml)
    (assert (modulo-hipotesis))
)

(defrule pregunta2
  ?ml <- (modulo-pregunta)
  ?dd <- (dd DENGUE)
  ?e <- (noDescartado DENGUE)
  =>
  (bind ?q (ask-question
    "¿Tienes ronchas?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q no)
      then
        (retract ?e)
    )
    (retract ?ml)
    (assert (modulo-hipotesis))
)

(defrule pregunta3
  ?ml <- (modulo-pregunta)
  ?dd <- (dd N1H1)
  ?e <- (noDescartado GRIPE)
  ?e1 <-(noDescartado N1H1)
  ?si <- (sin-enf (sintoma ?x))
  =>
  (bind ?q (ask-question
    "¿Llevas más de una semana con los sintomas?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q si)
      then
        (retract ?e)
        (assert (sin-enf (sintoma ?x) (duracion SEMANAS)))
      else
        (retract ?e1)
    )

    (retract ?ml)
    (assert (modulo-hipotesis))
)

;