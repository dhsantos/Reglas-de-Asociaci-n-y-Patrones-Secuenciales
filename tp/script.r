library (arules)
library (arulesViz)

path_resultados <- "~/Escritorio/Reglas de Asociacion/TP Reglas de Asociacion/datos"
path_archivos <- "~/Escritorio/Reglas de Asociacion/TP Reglas de Asociacion/datos"


################################################ PARTIDOS  #######################################################################

#La idea de este caso es sacar las relaciones 1 a 1 entre los partidos, si uno vota tal como vota el otro

#Interbloque Cambiemos comportamiento dentro del mismo bloque
basketPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketPartidos)
png(filename=paste(path_resultados, "soportesPartidos.png" , sep="/"))
itemFrequencyPlot(basketPartidos, names = FALSE, main="Distribucion del Soporte de los Partidos")
dev.off()
reglasPartidos <- apriori(basketPartidos , parameter=list(minlen=2, maxlen = 2, support=0.4,confidence = 0.75, target = "rules"))
# Ver si hace falta
#reglasPartidos <- reglasPartidos[is.redundant(x = reglasPartidos, measure = "confidence")]
inspect(sort(reglasPartidos, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasPartidos, file = paste(path_resultados, "reglasPartidos.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasPartidos, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasPartidos, method = "grouped matrix", engine = "interactive") #Las reglas en forma de grafo y matriz
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) #Las reglas en forma de matriz de colores no se si es util pero es colorido
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizPartidos.html" , sep="/"), selfcontained = FALSE)


################################################ Interbloque Cambiemos  ########################################################
cambiemos <- c("Coalicion Civica [NEGATIVO]","Coalicion Civica [POSITIVO]","Partido por la Justicia Social [NEGATIVO]","Partido por la Justicia Social [POSITIVO]","PRO [NEGATIVO]","PRO [POSITIVO]","Salta Somos Todos [POSITIVO]","Salta Somos Todos [NEGATIVO]","Union Civica Radical [NEGATIVO]","Union Civica Radical [POSITIVO]","Fte. Civico y Social de Catamarca [POSITIVO]","Fte. Civico y Social de Catamarca [NEGATIVO]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque

#Interbloque Cambiemos comportamiento dentro del mismo bloque
basketCambiemos <- read.transactions(paste(path_archivos, "transacciones_cambiemos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketCambiemos)
png(filename=paste(path_resultados, "soportesInterbloquesCambiemos.png" , sep="/"))
itemFrequencyPlot(basketCambiemos, names = FALSE, main="Distribucion del Soporte para el Interbloque Cambiemos")
dev.off()
reglasCambiemos <- apriori(basketCambiemos , parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasCambiemos, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasCambiemos, file = paste(path_resultados, "reglasInterbloqueCambiemos.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasCambiemos, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueCambiemos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasCambiemos, method = "grouped matrix", engine = "interactive") #Las reglas en forma de grafo y matriz
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueCambiemos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasCambiemos, method="matrix", engine = "interactive", measure=c("support","confidence")) #Las reglas en forma de matriz de colores no se si es util pero es colorido
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueCambiemos.html" , sep="/"), selfcontained = FALSE)



#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque
basketCambiemosPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketPartidos)
png(filename=paste(path_resultados, "soportesCambiemosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketCambiemosPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque Cambiemos con el resto de los partidos")
dev.off()
reglasCambiemosPartidos <- apriori(basketCambiemosPartidos , parameter=list(minlen=7, maxtime=50, support=0.35,confidence = 0.65, target = "rules"), appearance = list(lhs=cambiemos, default="rhs"))
write.PMML(reglasCambiemosPartidos, file = paste(path_resultados, "reglasCambiemosRestoPartidos.xml" , sep="/"))
p <- plot(reglasCambiemosPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueCambiemosRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasCambiemosPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueCambiemosRestoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasCambiemosPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueCambiemosRestoPartidos.html" , sep="/"), selfcontained = FALSE)





###########################################Interbloque FRENTE PARA LA VICTORIA–PJ #############################################
fpv <- c("Concertacion FORJA [AUSENTE]","Concertacion FORJA [POSITIVO]","Concertacion FORJA [ABSTENCION]","Concertacion FORJA [NEGATIVO]","Frente para la Victoria – PJ [NEGATIVO]","Frente para la Victoria – PJ [POSITIVO]","Frente para la Victoria – PJ [ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque FPV–PJ comportamiento dentro del mismo bloque
basketFpv <- read.transactions(paste(path_archivos, "transacciones_fpv.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketFpv)
png(filename=paste(path_resultados, "soportesInterbloquesFpv.png" , sep="/"))
itemFrequencyPlot(basketFpv, names = FALSE, main="Distribucion del Soporte para el Interbloque FPV-PJ")
dev.off()
reglasFpv <- apriori(basketFpv , parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasFpv, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasFpv, file = paste(path_resultados, "reglasInterbloqueFpv.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasCambiemos, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueFpv.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFpv, method = "grouped matrix", engine = "interactive") #Las reglas en forma de grafo y matriz
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueFpv.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasFpv, method="matrix", engine = "interactive", measure=c("support","confidence")) #Las reglas en forma de matriz de colores no se si es util pero es colorido
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueFpv.html" , sep="/"), selfcontained = FALSE)

#Interbloquecontra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketFpvPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketPartidos)
png(filename=paste(path_resultados, "soportesFpvRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketFpvPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque FPV-PJ con el resto de los partidos")
dev.off()
reglasFpvPartidos <- apriori(basketFpvPartidos , parameter=list(minlen=7, maxtime=50, support=0.35,confidence = 0.65, target = "rules"), appearance = list(lhs=fpv, default="rhs"))
write.PMML(reglasFpvPartidos, file = paste(path_resultados, "reglasFpvRestoPartidos.xml" , sep="/"))
p <- plot(reglasCambiemos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueFpvRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFpvPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueFpvRestoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasFpvPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueFpvRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE ARGENTINA FEDERAL #####################################################
af <- c("Cordoba Federal [NEGATIVO]","Cordoba Federal [EMPATE]","Cordoba Federal [AUSENTE]","Frente de la Concordia Misionero [AUSENTE]","Frente de la Concordia Misionero [POSITIVO]","Frente de la Concordia Misionero [NEGATIVO]","Justicialista [POSITIVO]","Justicialista [NEGATIVO]","Justicialista por Tucuman [AUSENTE]","Justicialista por Tucuman [POSITIVO]","Justicialista por Tucuman [NEGATIVO]","Partido Bloquista de San Juan [NEGATIVO]","Partido Bloquista de San Juan [POSITIVO]","Partido Bloquista de San Juan [AUSENTE]","Somos San Juan [AUSENTE]","Somos San Juan [NEGATIVO]","Somos San Juan [POSITIVO]","Todos Juntos por San Juan [AUSENTE]","Todos Juntos por San Juan [NEGATIVO]","Todos Juntos por San Juan [POSITIVO]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo


#Interbloque Argentina Federal comportamiento dentro del mismo bloque
basketArgFed <- read.transactions(paste(path_archivos, "transacciones_argfed.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketArgFed)
png(filename=paste(path_resultados, "soportesInterbloquesArgentinaFederal.png" , sep="/"))
itemFrequencyPlot(basketArgFed, names = FALSE, main="Distribucion del Soporte para el Interbloque Argentina Federal")
dev.off()
reglasArgFed <- apriori(basketArgFed, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasArgFed, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasArgFed, file = paste(path_resultados, "reglasInterbloqueArgFed.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasArgFed, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueArgFed.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasArgFed, method = "grouped matrix", engine = "interactive") #Las reglas en forma de grafo y matriz
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueArgFed.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasArgFed, method="matrix", engine = "interactive", measure=c("support","confidence")) #Las reglas en forma de matriz de colores no se si es util pero es colorido
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueArgFed.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketArgFedPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketArgFedPartidos)
png(filename=paste(path_resultados, "soportesArgFedPartidosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketArgFedPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque Argentina Federal con el resto de los partidos")
dev.off()
reglasArgFedPartidos <- apriori(basketArgFedPartidos , parameter=list(minlen=7, maxtime=50, support=0.35,confidence = 0.65, target = "rules"), appearance = list(lhs=af, default="rhs"))
write.PMML(reglasArgFedPartidos, file = paste(path_resultados, "reglasArgFedRestoPartidos.xml" , sep="/"))
p <- plot(reglasArgFedPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueArgFedRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasArgFedPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueCambiemosRestoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasArgFedPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueArgFedRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE FRENTE RENOVADOR UNA #################################################
fr <- c("Cordoba Trabajo y Produccion [POSITIVO]","Cordoba Trabajo y Produccion [ABSTENCION]","Cordoba Trabajo y Produccion [AUSENTE]","Cordoba Trabajo y Produccion [NEGATIVO]","Cultura Educacion y Trabajo  [POSITIVO]","Cultura Educacion y Trabajo  [NEGATIVO]","Cultura Educacion y Trabajo  [ABSTENCION]","Cultura Educacion y Trabajo  [AUSENTE]","Federal Unidos por una Nueva Argentina  [POSITIVO]","Federal Unidos por una Nueva Argentina  [NEGATIVO]","Federal Unidos por una Nueva Argentina  [ABSTENCION]","Trabajo y Dignidad [POSITIVO]","Trabajo y Dignidad [NEGATIVO]","Trabajo y Dignidad [ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque Argentina Federal comportamiento dentro del mismo bloque
basketFr <- read.transactions(paste(path_archivos, "transacciones_fruna.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketFr)
png(filename=paste(path_resultados, "soportesInterbloquesFrenteRenovador.png" , sep="/"))
itemFrequencyPlot(basketFr, names = FALSE, main="Distribucion del Soporte para el Interbloque Frente Renovador - UNA")
dev.off()
reglasFr <- apriori(basketFr, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasFr, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasFr, file = paste(path_resultados, "reglasInterbloqueFr.xml" , sep="/"))
p <- plot(reglasArgFed, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueFr.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFr, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueFr.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasFr, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueFr.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketFrPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketFrPartidos)
png(filename=paste(path_resultados, "soportesArgFedPartidosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketFrPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque Frente Renovador-UNA con el resto de los partidos")
dev.off()
reglasFrPartidos <- apriori(basketFrPartidos , parameter=list(minlen=7, maxtime=50, support=0.35,confidence = 0.65, target = "rules"), appearance = list(lhs=fr, default="rhs"))
write.PMML(reglasFrPartidos, file = paste(path_resultados, "reglasFrRestoPartidos.xml" , sep="/"))
p <- plot(reglasFrPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueFrRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasArgFedPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueFrRestoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasFrPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueFrRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE EN MARCHA ############################################################# 
marcha <- c("Libres del Sur [AUSENTE]","Libres del Sur [POSITIVO]","Libres del Sur [NEGATIVO]","Libres del Sur [ABSTENCION]","Peronismo para la Victoria [EMPATE]","Peronismo para la Victoria [NEGATIVO]","Peronismo para la Victoria [POSITIVO]","Peronismo para la Victoria [ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque En Marcha comportamiento dentro del mismo bloque
basketMarcha <- read.transactions(paste(path_archivos, "transacciones_enmarcha.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketMarcha)
png(filename=paste(path_resultados, "soportesInterbloquesEnMarcha.png" , sep="/"))
itemFrequencyPlot(basketMarcha, names = FALSE, main="Distribucion del Soporte para el Interbloque En Marcha")
dev.off()
reglasMarcha <- apriori(basketMarcha, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasMarcha, by = "lift"))
#plot(reglasCambiemos, method="grouped", measure="confidence")
write.PMML(reglasMarcha, file = paste(path_resultados, "reglasInterbloqueMarcha.xml" , sep="/"))
p <- plot(reglasMarcha, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueMarcha.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasMarcha, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueMarcha.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasMarcha, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueMarcha.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketMarchaPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',' , rm.duplicates = FALSE)
#itemFrequency(basketFrPartidos)
png(filename=paste(path_resultados, "soportesEnMarchaPartidosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketMarchaPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque En Marcha con el resto de los partidos")
dev.off()
reglasMarchaPartidos <- apriori(basketMarchaPartidos , parameter=list(minlen=7, maxtime=50, support=0.35,confidence = 0.65, target = "rules"), appearance = list(lhs=fr, default="rhs"))
write.PMML(reglasMarchaPartidos, file = paste(path_resultados, "reglasEnMarchaRestoPartidos.xml" , sep="/"))
p <- plot(reglasMarchaPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoInterbloqueEnMarchaRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasMarchaPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoInterbloqueEnMarchaRestoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasMarchaPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizInterbloqueEnMarchaRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################# PROVINCIAS ########################################################################

basketProvincias <- read.transactions(paste(path_archivos, "transacciones_provincias.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketProvincias)
png(filename=paste(path_resultados, "soportesProvincias.png" , sep="/"))
itemFrequencyPlot(basketProvincias, names = FALSE, main="Distribucion del Soporte para las Provincias")
dev.off()
reglasProvincias <- apriori(basketProvincias, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasProvincias, by = "lift"))
#plot(reglasProvincias, method="grouped", measure="confidence")
write.PMML(reglasProvincias, file = paste(path_resultados, "reglasProvincias.xml" , sep="/"))
p <- plot(reglasProvincias, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoProvincias.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasProvincias, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoProvincias.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasProvincias, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizProvincias.html" , sep="/"), selfcontained = FALSE)



########################################## PROVINCIAS y PARTIDOS ####################################################################

basketProvinciasPartidos <- read.transactions(paste(path_archivos, "transacciones_provincias_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
#itemFrequency(basketProvinciasPartidos)
png(filename=paste(path_resultados, "soportesProvincias.png" , sep="/"))
itemFrequencyPlot(basketProvinciasPartidos, names = FALSE, main="Distribucion del Soporte para las Provincias/ Partidos")
dev.off()
reglasProvinciasPartidos <- apriori(basketProvinciasPartidos, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasProvinciasPartidos, by = "lift"))
#plot(reglasProvinciasPartidos, method="grouped", measure="confidence")
write.PMML(reglasProvinciasPartidos, file = paste(path_resultados, "reglasProvinciasPartidos.xml" , sep="/"))
p <- plot(reglasProvincias, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoProvinciasPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasProvinciasPartidos, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoProvinciasPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasProvinciasPartidos, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizProvinciasPartidos.html" , sep="/"), selfcontained = FALSE)


########################################## APROBACIONES DE LEYES ####################################################################
leyes <- c("LEY_APROBADA","LEY_RECHAZADA")

basketLeyes <- read.transactions(paste(path_archivos, "transacciones.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE, appearance = list(default="lhs", rhs=leyes))
#itemFrequency(basketLeyes)
png(filename=paste(path_resultados, "soportesLeyes.png" , sep="/"))
itemFrequencyPlot(basketLeyes, names = FALSE, main="Distribucion del Soporte para las Leyes tratadas")
dev.off()
reglasLeyes <- apriori(basketLeyes, parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.65, target = "rules"))
inspect(sort(reglasLeyes, by = "lift"))
#plot(reglasLeyes, method="grouped", measure="confidence")
write.PMML(reglasLeyes, file = paste(path_resultados, "reglasLeyes.xml" , sep="/"))
p <- plot(reglasLeyes, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados, "grafoLeyes.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasLeyes, method = "grouped matrix", engine = "interactive") 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizGrafoLeyes.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasLeyes, method="matrix", engine = "interactive", measure=c("support","confidence")) 
htmlwidgets::saveWidget(p, paste(path_resultados, "matrizLeyes.html" , sep="/"), selfcontained = FALSE)



#################################################################################################################################
#Cosas por las dudas
reglasCambiemos <- apriori(basketPartidos , parameter=list(minlen=2, maxtime=50, support=0.25,confidence = 0.75, target = "rules"), appearance = list(rhs= c("Salta Somos Todos [NEGATIVO]", "Salta Somos Todos [POSITIVO]")))
reglasCambiemos <- reglasCambiemos[is.redundant(x = reglasCambiemos, measure = "confidence")]
reglasCambiemos <- reglasCambiemos[is.maximal(reglasCambiemos)]
reglasCambiemos <- subset(reglasCambiemos, (rhs %ain% cambiemos), (lhs %ain% cambiemos))
p <- plotly_arules(basketCambiemos)
browseURL("arules.html")


