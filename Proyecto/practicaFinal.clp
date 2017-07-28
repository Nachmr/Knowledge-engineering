(deftemplate sintomas
   		(multifield nombre)
   		(slot presencia
			(type SYMBOL)
			(allowed-symbols N S NS)
			(default NS)
   		)
		(slot descripcion
			(type STRING)
		)
   		(slot dolor
   			(type SYMBOL)
			(allowed-symbols S N NS)
			(default NS)
  	 	)
)


(deftemplate hipotesis-principal
	(multifield enfermedad)
)

(deftemplate diferencial
	(multifield enfermedad)
)

(deftemplate diagnostico
	(multifield enfermedad)
	(multifield razonamiento)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Sintomas de las enfermedades
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts listasintomas
 (sintomas (nombre Disuria) (descripcion ": Dolor al orinar"))
 (sintomas (nombre Enrojecimiento))
 (sintomas (nombre "Miccion frecuente"))
 (sintomas (nombre "Dolor anal"))
 (sintomas (nombre Sangrado))
 (sintomas (nombre Tenesmo) (descripcion ": Sensación de necesidad de defecar"))
 (sintomas (nombre "Dolor al tragar"))
 (sintomas (nombre Inflamacion))
 (sintomas (nombre Supuracion) (descripcion ": Herida con pus"))
 (sintomas (nombre Ulcera) (descripcion ": Herida con forma de crater"))
 (sintomas (nombre Sarpullido))
 (sintomas (nombre "Dolor oseo"))
 (sintomas (nombre "Dolor abdominal"))
 (sintomas (nombre "Olor vaginal"))
 (sintomas (nombre "Dolor ciatica"))
 (sintomas (nombre "Aumento o alteracion flujo vaginal"))
 (sintomas (nombre Verrugas))
 (sintomas (nombre "Secrecion blanca"))
 (sintomas (nombre Dispaneunia)(descripcion ": coito doloroso"))
 (sintomas (nombre "Ganglios")(descripcion " (inflamados o duros)"))
 (sintomas (nombre "Secrecion escasa"))
 (sintomas (nombre Erupcion))
 (sintomas (nombre "Dolor vaginal"))
 (sintomas (nombre Manchas))
 (sintomas (nombre Gomas) (descripcion ": Tumefaccion"))
 (sintomas (nombre Paralisis))
 (sintomas (nombre Burbujas)(descripcion ": Pompas en la piel"))
 (sintomas (nombre "Menstruacion interrumpida"))
 (sintomas (nombre "Rasgos raros")(descripcion ": frente abombada, nariz de silla..."))
 (sintomas (nombre "Bajo peso"))
 (sintomas (nombre Prurito)(descripcion ": hormigueo peculiar o irritación incómoda de la piel"))
 (sintomas (nombre "Anormalidades oseas")(descripcion ": normalmente huesos con formas extrañas"))

 (hipotesis-principal (enfermedad indefinido))
 (diferencial (enfermedad indefinido))
 (diagnostico (enfermedad indefinido))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;    Reglas generales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule actualizar-ulcera
		(declare (salience 501))
		?borrar <-(sintomas (nombre ?nom)(presencia S)(dolor NS))
		(sintomas (nombre ?nom)(presencia S)(dolor S | N))
		=>
		(retract ?borrar)
)

(defrule actualizar-sintoma
		(declare (salience 501))
		?borrar <-(sintomas (nombre ?nom)(presencia NS))
		(sintomas (nombre ?nom)(presencia S | N)(dolor S | N | NS))
		=>
		(retract ?borrar)
)

(defrule actualizar-principal
		(declare (salience 501))
		?borrar <-(hipotesis-principal (enfermedad ?nom))
		(hipotesis-principal (enfermedad ?enf))
		(neq ?nom ?enf)
		=>
		(retract ?borrar)
)

(defrule actualizar-diferencial
		(declare (salience 501))
		?borrar <-(diferencial (enfermedad ?nom))
		(diferencial (enfermedad ?enf))
		(neq ?nom ?enf)
		=>
		(retract ?borrar)
)

(defrule actualizar-diagnostico
		(declare (salience 501))
		?borrar <-(diagnostico (enfermedad ?nom))
		(diagnostico (enfermedad ?enf))
		(neq ?nom ?enf)
		=>
		(retract ?borrar)
)


(defrule dar-diagnostico
	(declare (salience 999))
	(print-diagnostico)
	(diagnostico (enfermedad ?enf)(razonamiento ?des))
	=>
	(printout t "Diagnostico: " ?enf crlf)
	(printout t "Motivos: " ?des crlf)
	(halt)
)

(defrule muestra-lista
	(declare (salience 500))
  	(print-list list)
  	(sintomas (nombre ?name)(presencia NS) (descripcion ?des))
  	=>
  	(printout t "       Sintoma  " ?name)
	(printout t ?des crlf)
	(assert (introduce-sintoma))
	(assert (borrar-print))
)	

(defrule borrar-list
	?borrar<-(borrar-print)
	?borrar2<-(print-list list)
	=>
	(retract ?borrar)
	(retract ?borrar2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Introducir sintoma y entrar en el modulo de hipotesis principal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule introducir-sintoma
	?borrar<-(introduce-sintoma)
	=>
	(retract ?borrar)
	(assert (sintomas (nombre =(read))(presencia S)))
	(assert (modulo hipotesis-principal))
)

(defrule start
	(declare (salience 2000))
  	?init <- (initial-fact)
  	=>
	(printout t "Sistema experto para diagnosticar enfermedades de transmisión sexual" crlf)
	(printout t "Seleccione un sintoma de la lista (si no sufre ninguno, escriba "Fin"):" crlf)
	(retract ?init)
	(assert (print-list list))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;   Modulo hipotesis principal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule preguntar-inflamacion
	(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S))
	(sintomas (nombre Inflamacion)(presencia NS))
	=>
	(printout t "¿Tiene inflamacion? (S/N)"crlf)
	(assert (sintomas (nombre Inflamacion)(presencia =(read))))
)

(defrule preguntar-pasises-tropicales
	(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor N))
	(sintomas (nombre Inflamacion)(presencia N))
	=>
	(printout t "¿Ha estado en paises tropicales o subtropicales? (S/N)"crlf)
	(assert (paises tropicales =(read)))
)

(defrule hipotesis-principal-uretritis
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Miccion dolorosa")(presencia S))
	(sintomas (nombre Supuracion)(presencia S))
	(sintomas (nombre "Miccion frecuente")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad uretritis)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-cervicitis
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Aumento o alteracion flujo vaginal")(presencia S))
	(sintomas (nombre "Dolor abdominal")(presencia S))
	(sintomas (nombre Dispaneunia)(presencia S))
	(sintomas (nombre "Menstruacion interrumpida")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad cervicitis)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-ulcera
	(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor NS))
	=>
	(printout t "¿La ulcera es dolorosa? (S/N)"crlf)
	(assert (sintomas (nombre Ulcera)(presencia S)(dolor =(read))))
)

(defrule hipotesis-principal-vulvovaginitis-tricoma
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Aumento o alteracion flujo vaginal")(presencia S))
	(sintomas (nombre Prurito)(presencia S))
	(sintomas (nombre Disuria)(presencia S))
	(sintomas (nombre "Dolor abdominal")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad tricoma)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-vulvovaginitis-bacteriana
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Olor vaginal")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad bacteriana)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-vulvovaginitis-candida
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Dispaneunia)(presencia S))
	(sintomas (nombre "Dolor vaginal")(presencia S))
	(sintomas (nombre "Secrecion escasa")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad candida)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-faringitis
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Dolor al tragar")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad faringitis)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-chancroide
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor S))
	(sintomas (nombre "Dolor abdominal")(presencia S))
	(sintomas (nombre Inflamacion)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad chancroide)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-linfogranuloma1
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor N))
	(sintomas (nombre Ganglios)(presencia S))
	(paises tropicales S)
	=>
	(assert (hipotesis-principal (enfermedad linfogranuloma1)))
	(retract ?borrar)
	(assert(modulo diferencial))
)
(defrule hipotesis-principal-granuloma
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor N))
	(sintomas (nombre Inflamacion)(presencia N))
	(paises tropicales S)
	=>
	(assert (hipotesis-principal (enfermedad granuloma)))
	(retract ?borrar)
	(assert(modulo diferencial))
)


(defrule preguntar-tiempo-enfermo
	(modulo hipotesis-principal)
	(sintomas (nombre Ganglios)(presencia S))
	(sintomas (nombre Ulcera)(presencia S)(dolor N))
	(paises tropicales S)
	=>
	(printout t "¿Cuantas semanas lleva enfermo?" crlf)
	(assert (enfermo =(read)))
)

(defrule hipotesis-principal-linfogranuloma2
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S)(dolor N))
	(sintomas (nombre Ganglios)(presencia S))
	(sintomas (nombre "Dolor abdominal")(presencia S))
	(enfermo ?n)
	(test(> ?n 2))
	=>
	(assert (hipotesis-principal (enfermedad linfogranuloma2)))
	(retract ?borrar)
	(assert(modulo diferencial))
)


(defrule hipotesis-principal-verrugas
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Verrugas)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad verrugas)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-herpes
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Burbujas)(presencia S))
	(sintomas (nombre "Dolor ciatica")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad herpes)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-pediculosis
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Sarpullido)(presencia S))
	(sintomas (nombre Picor)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad pediculosis)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-sifilis-precoz-primario
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Ulcera)(presencia S))
	(sintomas (nombre Ganglios)(presencia S))
	(sintomas (nombre Inflamacion)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad sifilis-precoz-primario)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-sarna
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Erupcion)(presencia S))
	(sintomas (nombre Picor)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad sarna)))
	(retract ?borrar)
	(assert(modulo diferencial))
)


(defrule hipotesis-principal-sifilis-precoz-secundario
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Manchas)(presencia S))
	(sintomas (nombre Verrugas)(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad sifilis-precoz-secundario)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-sifilis-tardia-terciaria
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre Paralisis)(presencia S))
	(sintomas (nombre Gomas)(presencia S))
	(sintomas (nombre "Dolor oseo")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad sifilis-tardia-terciaria)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule probable-hipotesis-principal-sifilis-congenita-temprana
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Bajo peso")(presencia S))
	(sintomas (nombre "Anormalidades oseas")(presencia S))
	=>
	(assert (pregunta-edad))
	(assert (posible-sifilis))
)

(defrule pregunta-edad
	(modulo hipotesis-principal)
	(pregunta-edad)
	=>
	(printout t "¿Cuantos años tiene?" crlf)
	(assert(edad =(read)))
)

(defrule hipotesis-principal-sifilis-congenita-temprana
	?borrar<-(modulo hipotesis-principal)
	(posible-sifilis)
	(edad ?n)
	(test(< ?n 2))
	=>
	(assert (hipotesis-principal (enfermedad sifilis-congenita-temprana)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule hipotesis-principal-sifilis-congenita-tardia
	?borrar<-(modulo hipotesis-principal)
	(sintomas (nombre "Rasgos raros")(presencia S))
	=>
	(assert (hipotesis-principal (enfermedad sifilis-congenita-tardia)))
	(retract ?borrar)
	(assert(modulo diferencial))
)

(defrule pasar-a-diferencial
	?borrar<-(modulo hipotesis-principal)
	=>
	(retract ?borrar)
	(assert(modulo diferencial))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;    Modulo hipotesis diferencial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule no-hay-diferencial
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sifilis-congenita-tardia))
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad sifilis-congenita-tardia)(razonamiento "Tiene rasgos raros.")))
	(assert(print-diagnostico))
)

(defrule no-hay-diferencialsifilisc
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sifilis-congenita-temprana))
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad sifilis-congenita-temprana)(razonamiento "Bajo peso, anormalidades oseas y es menor de 2 años.")))
	(assert(print-diagnostico))
)

(defrule no-hay-diferencialifilist
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sifilis-tardia-terciaria))
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad sifilis-tardia-terciaria)(razonamiento "Tiene gomas, dolor oseo y paralisis.")))
	(assert(print-diagnostico))
)

(defrule no-hay-diferencialsifilisps
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sifilis-precoz-secundario))
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad sifilis-precoz-secundario)(razonamiento "Tiene manchas y verrugas.")))
	(assert(print-diagnostico))
)

(defrule no-hay-diferencialsifilispp
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sifilis-precoz-primario))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad "Sifilis precoz en estado primario")(razonamiento "Tiene úlcera, inflamación y enrojecimiento.")))
	(assert(print-diagnostico))
)

(defrule diagnostico-herpes
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad herpes))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad Herpes)(razonamiento "Tiene burbujas y dolor de ciatica.")))
	(assert(print-diagnostico))
)

(defrule posible-faringitis
	(modulo diferencial)
	(hipotesis-principal (enfermedad faringitis))
	=>
	(printout t "¿Realiza sexo oral?(S/N)"crlf)
	(assert(sexo-oral =(read)))
)

(defrule diagnostico-faringitis
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad faringitis))
	(sexo-oral S)
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad faringitis)(razonamiento "Tiene dolor e inflamación de garganta, además realiza sexo oral.")))
	(assert(print-diagnostico))
)

(defrule diagnostico-no-faringitis
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad faringitis))
	(sexo-oral N)
	=>
	(retract ?borrar)
	(assert (diagnostico (enfermedad "Sin diagnostico")(razonamiento "Aunque podría parecer faringitis (de tranmision sexual), si no realiza sexo oral no creo que lo sea.")))
	(assert(print-diagnostico))
)

(defrule diferencial-sifilischancroide
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad chancroide))
	=>
	(assert (diferencial (enfermedad sifilis-precoz-primario)))
	(retract ?borrar)
	(assert(modulo diagnostico))
)

(defrule diferencial-sifilisverrugas
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad verrugas))
	=>
	(assert (diferencial (enfermedad sifilis-precoz-primario)))
	(retract ?borrar)
	(assert(modulo diagnostico))
)

(defrule diferencial-linfogranuloma
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad granuloma))
	=>
	(assert (diferencial (enfermedad linfogranuloma1)))
	(retract ?borrar)
	(assert(modulo diagnostico))
)

(defrule diferencial-granuloma
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad linfogranuloma1))
	=>
	(assert (diferencial (enfermedad granuloma)))
	(retract ?borrar)
	(assert(modulo diagnostico))
)

(defrule posible-pediculosis
	(modulo diferencial)
	(hipotesis-principal (enfermedad pediculosis))
	=>
	(printout t "Observese bien, ¿ve algún tipo de micro-irritacion? (S/N)" crlf)
	(assert(liendres =(read)))
)

(defrule diagnostico-pediculosis
	?borrar<-(modulo diferencial)
	(liendres N)
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad pediculosis)(razonamiento "Sarpullido y picor.")))
	(assert(print-diagnostico))
)

(defrule diagnostico-no-pediculosis
	?borrar<-(modulo diferencial)
	(liendres S)
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad "Sin diagnostico")(razonamiento "No padece ninguna enfermedad de las posibles, es posible que padezca de ladillas o liendres")))
	(assert(print-diagnostico))
)

(defrule diagnostico-sarna
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad sarna))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad sarna)(razonamiento "Erupcion y picor")))
	(assert(print-diagnostico))
)


(defrule diagnostico-tricoma
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad tricoma))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad tricoma)(razonamiento "Aumento flujo vaginal, prurito y disuria" )))
	(assert(print-diagnostico))
)

(defrule diagnostico-bacteriana
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad bacteriana))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad bacteriana)(razonamiento "Olor vaginal")))
	(assert(print-diagnostico))
)

(defrule diagnostico-candida
	?borrar<-(modulo diferencial)
	(hipotesis-principal (enfermedad candida))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad candida)(razonamiento "Dispaneunia, dolor vaginal y Secrecion escasa")))
	(assert(print-diagnostico))
)

(defrule pasar-diagnostico
	?borrar<-(modulo diferencial)
	=>
	(retract ?borrar)
	(assert(modulo diagnostico))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Modulo diagnostico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(defrule granuloma-vs-linfogranuloma
	(modulo diagnostico)
	(hipotesis-principal (enfermedad linfogranuloma1))
	(diferencial (enfermedad granuloma))
	=>
	(printout t "¿Sufre dolor abdominal?(S/N)" crlf)
	(assert(sintomas(nombre "Dolor abdominal")(presencia =(read))))
)


(defrule diagnostico-granuloma
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad granuloma))
	(diferencial (enfermedad linfogranuloma1))
	(sintomas(nombre "Dolor abdominal")(presencia N))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad granuloma)(razonamiento "Úlcera, no tiene inflamación y no padece dolor abdominal, además ha viajado a paises tropicales o subtropicales.")))
	(assert(print-diagnostico))
)

(defrule granuloma-vs-linfogranuloma2
	(modulo diagnostico)
	(hipotesis-principal (enfermedad granuloma))
	(diferencial (enfermedad linfogranuloma1))
	=>
	(printout t "¿Sufre dolor abdominal?(S/N)" crlf)
	(assert(sintomas(nombre "Dolor abdominal")(presencia =(read))))
)

(defrule diagnostico-linfogranuloma1
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad linfogranuloma1))
	(diferencial (enfermedad granuloma))
	(sintomas(nombre "Dolor abdominal")(presencia S))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad linfogranuloma1)(razonamiento "Úlcera indolora, gánglios duros y padece dolor abdominal, además ha viajado a paises tropicales o subtropicales.")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-uretritis
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad uretritis))
	(diferencial (enfermedad indefinido))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad Uretritis)(razonamiento "Problemas al miccionar y supuracion.")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-tricoma
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad tricoma))
	(diferencial (enfermedad indefinido))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad tricoma)(razonamiento "Aumento flujo vaginal, prurito y disuria.")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-bacteriana
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad bacteriana))
	(diferencial (enfermedad indefinido))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad bacteriana)(razonamiento "Olor vaginal.")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-candida
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad candida))
	(diferencial (enfermedad indefinido))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad candida)(razonamiento "Dispaneunia, dolor vaginal y Secrecion escasa.")))
	(assert(print-diagnostico))
)

(defrule posible-chancroide
	(modulo diagnostico)
	(hipotesis-principal (enfermedad chancroide))
	(diferencial (enfermedad sifilis-congenita-temprana))
	=>
	(printout t "¿Tiene ganglios inflamados?(S/N)" crlf)
	(assert(sintomas(nombre Ganglios)(presencia =(read))))
)

(defrule diagnosticar-chancroide
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad chancroide))
	(sintomas(nombre Ganglios)(presencia N))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad Chancroide)(razonamiento "Úlcera, inflamación, enrojecimiento y no tiene los ganglios duros ni inflamados")))
	(assert(print-diagnostico))
)


(defrule diagnostico-sifilis3
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad verrugas))
	(diferencial (enfermedad sifilis-precoz-primario))
	(sintomas (nombre Manchas)(presencia S))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad "Sifilis precoz en estado primario")(razonamiento "Verrugas y manchas")))
	(assert(print-diagnostico))
)

(defrule verrugas-vs-sifilis
	(modulo diagnostico)
	(hipotesis-principal (enfermedad verrugas))
	(diferencial (enfermedad sifilis-precoz-primario))
	(sintomas (nombre Manchas)(presencia NS))
	=>
	(printout t "¿Tiene usted manchas?(S/N)"crlf)
	(assert(sintomas (nombre Manchas)(presencia =(read))))
)

(defrule diagnostico-verrugas
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad verrugas))
	(diferencial (enfermedad sifilis-precoz-primario))
	(sintomas (nombre Manchas)(presencia N))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad "Verrugas genitales")(razonamiento "Verrugas y no tiene manchas")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-sano
	?borrar<-(modulo diagnostico)
	(diagnostico (enfermedad indefinido))
	(sintomas (nombre Fin))
	=>
	(retract ?borrar)
	(assert(diagnostico (enfermedad "Sin diagnostico")(razonamiento "No padece ninguna enfermedad de las posibles")))
	(assert(print-diagnostico))
)

(defrule diagnosticar-siflis-primario
	?borrar<-(modulo diagnostico)
	(hipotesis-principal (enfermedad chancroide))
	(sintomas(nombre Ganglios)(presencia S))
	=>
	(retract ?borrar)
	(assert(diagnostico(enfermedad "Sifilis precoz en estado primario")(razonamiento "Úlcera, inflamación, enrojecimiento y tiene los ganglios duros o inflamados")))
	(assert(print-diagnostico))
)


(defrule preguntar-sintoma
	?borrar<-(modulo diagnostico)
	=>
	(retract ?borrar)
	(assert(preguntar-sintoma))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Preguntar nuevo sintoma y pasar a hipotesis principal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule pedir-nuevo-sintoma
	?borrar<-(preguntar-sintoma)
	=>
	(retract ?borrar)
	(printout t "Digame algún sintoma más (si no tiene mas escriba "Fin"):" crlf)
	(assert(print-list list))
)