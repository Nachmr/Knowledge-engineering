(deffacts familia
	(casado "Antonio Miguel" Antonia)
	(casado Antonio Angeles)
	(casado "Jose Bena" Bernardina)
	(casado "Jose H.padre" Concepcion)
	(casado "Jose Estrella" Estrella)
	(casado Angel AntoniaAbuela)
	(casado Pepe EstrellaAbuela)
	
	(hijo-de Nacho Antonia)
	(hijo-de Pilar Antonia)
	(hijo-de Carmen Antonia)
	(hijo-de Maria Antonia)
	
	(hijo-de Marta Bernardina)
	
	(hijo-de Cecilia Estrella)
	(hijo-de Pablo Estrella)
	(hijo-de Daniel Estrella)
	
	(hijo-de Miguel Angeles)
	(hijo-de Carolina Angeles)
	
	(hijo-de Juan Paco)
	(hijo-de Violeta Paco)
	
	(hijo-de Laura Concepcion)

	(hijo-de "Antonio Miguel" EstrellaAbela)
	(hijo-de "Jose H.Padre" EstrellaAbela)
	(hijo-de Paco EstrellaAbela)
	(hijo-de Estrella EstrellaAbela)

	(hijo-de Antonia AntoniaAbuela)
	(hijo-de Angeles AntoniaAbuela)
	(hijo-de Bernardina AntoniaAbuela)

	(hombre Nacho)
	(hombre Pablo)
	(hombre Daniel)
	(hombre Paco)
	(hombre Miguel)
	(hombre "Jose Bena")
	(hombre "Jose H.Padre")
	(hombre "Jose Estrella")
	(hombre Pepe)
	(hombre Angel)
	(hombre Paco)
	(hombre Juan)
	(hombre "Antonio Miguel")
	(hombre Antonio)
	

	(mujer Antonia)
	(mujer Angeles)
	(mujer Bernardina)
	(mujer Concepcion)
	(mujer AntoniaAbuela)
	(mujer EstrellaAbuela)
	(mujer Estrella)
	(mujer Pilar)
	(mujer Carmen)
	(mujer Maria)
	(mujer Marta)
	(mujer Cecilia)
	(mujer Carolina)
	(mujer Violeta)
	(mujer Laura)
)

	
(defrule lee-entrada
     (initial-fact)
      =>
     (printout t "Introduzca un nombre" crlf)
     (assert (nombreentrada (read)))
)


(defrule padre
	(casado ?esposo ?esposa)
	(hijo-de ?hijo ?esposa)
	=>
	(assert (hijo-de ?hijo ?esposo))
)

(defrule padre2
	(casado ?esposo ?esposa)
	(hijo-de ?hijo ?esposo)
	=>
	(assert (hijo-de ?hijo ?esposa))
)

(defrule hermanos
	(hijo-de ?hijo1 ?esposa)
	(hijo-de ?hijo ?esposa)
	=>
	(assert (hermanos ?hijo1 ?hijo))
	(assert (hermanos ?hijo ?hijo1))
)

(defrule tios
	(hijo-de ?hijo ?madre)
	(hermanos ?madre ?a)

	=>
	(assert (tio  ?hijo ?a))
)

(defrule abuelos
	(hijo-de ?x ?y)
        (hijo-de ?y ?z)
        =>
        (assert (abuelo ?x ?z))
)

(defrule primos
	(hijo-de ?a ?b)
	(tio ?c ?b)
	=>
	(assert (primo ?a ?c))
	(assert (primo ?c ?a))

)


(defrule eshijos
	(nombreentrada ?nombre)
	(hijo-de ?nombre ?madre)
	=> (printout t ?nombre " es hijo/a de " ?madre crlf)
)

(defrule quitarhrep
	?i <- (hermanos ?a ?a)
	=>
	(retract ?i)
)

(defrule quitarprihrep
	?i <- (primo ?a ?a)
	=>
	(retract ?i)
)

(defrule quitarpadretio
	(tio ?a ?b)
	(hijo-de ?a ?b)
	?i <- (tio ?a ?b)
	=>
	(retract ?i)
)

(defrule quitarprimoyhermano
	(primo ?a ?b)
	(hermanos ?a ?b)
	?i <- (primo ?a ?b)
	=>
	(retract ?i)
)
	

(defrule eshermano
	(nombreentrada ?nombre)
	(hermanos ?nombre ?hermano)
	=> (printout t ?nombre " es hermano/a de " ?hermano crlf)
)

(defrule estio
	(nombreentrada ?nombre)
	(tio ?nombre ?a)
	=> (printout t ?a " es tio/a de " ?nombre crlf)
)

(defrule esabuelo
	(nombreentrada ?nombre)
	(abuelo ?nombre ?a)
	=> (printout t ?a " es abuelo/a de " ?nombre crlf)
)

(defrule esprimo
	(nombreentrada ?nombre)
	(primo ?nombre ?a)
	=> (printout t ?nombre " es primo de " ?a crlf)
)



