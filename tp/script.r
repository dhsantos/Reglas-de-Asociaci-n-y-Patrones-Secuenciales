library (arules)
library (arulesViz)
library(colorspace)

path_archivos <- "~/Reglas-de-Asociaci-n-y-Patrones-Secuenciales/tp/datos"
path_resultados_xml <- "~/Reglas-de-Asociaci-n-y-Patrones-Secuenciales/tp/xmlReglas"
path_resultados_html <- "~/Reglas-de-Asociaci-n-y-Patrones-Secuenciales/tp/informes genenciales/src"
path_resultados_graficos <- "~/Reglas-de-Asociaci-n-y-Patrones-Secuenciales/tp/graficos"


################################################ PARTIDOS  #######################################################################

#La idea de este caso es sacar las relaciones 1 a 1 entre los partidos, si uno vota tal como vota el otro

basketPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketPartidos)
png(filename=paste(path_resultados_graficos, "soportesPartidos.png" , sep="/"))
itemFrequencyPlot(basketPartidos, decreasing=TRUE , names = FALSE, main="Distribucion del Soporte de los Partidos")
dev.off()
reglasPartidos <- apriori(basketPartidos , parameter=list(minlen=2, maxlen = 2, support=0.60,confidence = 0.90, target = "rules"))
gi <- generatingItemsets(reglasPartidos)
d <- which(duplicated(gi))
reglasPartidos <- reglasPartidos[-d]
inspect(sort(reglasPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordPartidos.png" , sep="/"))
plot(head(sort(reglasPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE))
dev.off()
write.PMML(reglasPartidos, file = paste(path_resultados_xml, "reglasPartidos.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasPartidos, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoPartidos1a1.html" , sep="/"), selfcontained = FALSE)
#p <-plot(reglasPartidos, method = "grouped matrix", engine = "htmlwidget") #Las reglas en forma de grafo y matriz
#htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizGrafoPartidos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) #Matriz de bloques
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizPartidos.html" , sep="/"), selfcontained = FALSE)


#Entre grupo de partidos

basketPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketPartidos)
png(filename=paste(path_resultados_graficos, "soportesPartidos.png" , sep="/"))
itemFrequencyPlot(basketPartidos, decreasing=TRUE , names = FALSE, main="Distribucion del Soporte de los Partidos")
dev.off()
reglasPartidos <- apriori(basketPartidos , parameter=list(minlen=2, maxlen = 15, support=0.65,confidence = 0.80, target = "rules"))
gi <- generatingItemsets(reglasPartidos)
d <- which(duplicated(gi))
reglasPartidos <- reglasPartidos[-d]
inspect(sort(reglasPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordPartidosGrupos.png" , sep="/"))
plot(head(sort(reglasPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE))
dev.off()
write.PMML(reglasPartidos, file = paste(path_resultados_xml, "reglasPartidosGrupos.xml" , sep="/")) #Se guardan las reglas en xml
p <- plot(reglasPartidos, method = "graph", engine = "htmlwidget") #Las reglas en forma de grafo
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoPartidosGrupos.html" , sep="/"), selfcontained = FALSE)
#p <-plot(reglasPartidos, method = "grouped matrix", engine = "htmlwidget") #Las reglas en forma de grafo y matriz
#htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizGrafoPartidosGrupos.html" , sep="/")), selfcontained = FALSE)
p <-plot(reglasPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) #Matriz de bloques
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizPartidosGrupos.html" , sep="/"), selfcontained = FALSE)



################################################ Interbloque Cambiemos  ########################################################
cambiemos <- c("Coalicion Civica[NEGATIVO]","Coalicion Civica[POSITIVO]","Partido por la Justicia Social[NEGATIVO]","Partido por la Justicia Social[POSITIVO]","PRO[NEGATIVO]","PRO[POSITIVO]","Salta Somos Todos[POSITIVO]","Salta Somos Todos[NEGATIVO]","Union Civica Radical[NEGATIVO]","Union Civica Radical[POSITIVO]","Fte. Civico y Social de Catamarca[POSITIVO]","Fte. Civico y Social de Catamarca[NEGATIVO]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque

#Interbloque Cambiemos comportamiento dentro del mismo bloque
basketCambiemos <- read.transactions(paste(path_archivos, "transacciones_cambiemos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketCambiemos)
png(filename=paste(path_resultados_graficos, "soportesCambiemos.png" , sep="/"))
itemFrequencyPlot(basketCambiemos, names = FALSE, main="Distribucion del Soporte 1-itemset para el Interbloque Cambiemos")
dev.off()
reglasCambiemos <- apriori(basketCambiemos , parameter=list(minlen=6, maxtime=50, support=0.1,confidence = 0.80, target = "rules"))
gi <- generatingItemsets(reglasCambiemos)
d <- which(duplicated(gi))
reglasCambiemos <- reglasCambiemos[-d]
inspect(sort(reglasCambiemos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordInterbloquesCambiemos.png" , sep="/"))
plot(reglasCambiemos, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasCambiemos, file = paste(path_resultados_xml, "reglasCambiemos.xml" , sep="/")) 
p <- plot(reglasCambiemos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoCambiemos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasCambiemos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200)))
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizCambiemos.html" , sep="/"), selfcontained = FALSE)



#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque
basketCambiemosPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketCambiemosPartidos)
png(filename=paste(path_resultados_graficos, "soportesCambiemosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketCambiemosPartidos, names = FALSE, main="Distribucion del Soporte 1-itemset para el Interbloque Cambiemos con el resto de los partidos")
dev.off()
reglasCambiemosPartidos <- apriori(basketCambiemosPartidos , parameter=list(minlen=7, maxtime=50, support=0.2,confidence = 0.80, target = "rules"), appearance = list(lhs=cambiemos, default="rhs"))
inspect(sort(reglasCambiemosPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordCambiemosRestoPartidos.png" , sep="/"))
plot(reglasCambiemosPartidos, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasCambiemosPartidos, file = paste(path_resultados_xml, "reglasCambiemosRestoPartidos.xml" , sep="/"))
p <- plot(reglasCambiemosPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueCambiemosRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasCambiemosPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueCambiemosRestoPartidos.html" , sep="/"), selfcontained = FALSE)



###########################################Interbloque FRENTE PARA LA VICTORIA–PJ #############################################
fpv <- c("Concertacion FORJA[AUSENTE]","Concertacion FORJA[POSITIVO]","Concertacion FORJA[ABSTENCION]","Concertacion FORJA[NEGATIVO]","Frente para la Victoria - PJ[NEGATIVO]","Frente para la Victoria - PJ[POSITIVO]","Frente para la Victoria - PJ[ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque FPV–PJ comportamiento dentro del mismo bloque
basketFpv <- read.transactions(paste(path_archivos, "transacciones_fpv.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketFpv)
png(filename=paste(path_resultados_graficos, "soportesInterbloquesFpv.png" , sep="/"))
itemFrequencyPlot(basketFpv, names = FALSE, main="Distribucion del Soporte 1-itemset para el Interbloque FPV-PJ")
dev.off()
reglasFpv <- apriori(basketFpv , parameter=list(minlen=2, maxtime=50, support=0.1,confidence = 0.80, target = "rules"))
gi <- generatingItemsets(reglasFpv)
d <- which(duplicated(gi))
reglasFpv <- reglasFpv[-d]
inspect(sort(reglasFpv, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordFpv.png" , sep="/"))
plot(reglasFpv, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasFpv, file = paste(path_resultados_xml, "reglasInterbloqueFpv.xml" , sep="/")) 
p <- plot(reglasFpv, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueFpv.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFpv, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueFpv.html" , sep="/"), selfcontained = FALSE)

#Interbloquecontra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketFpvPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketFpvPartidos)
png(filename=paste(path_resultados_graficos, "soportesFpvRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketFpvPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque FPV-PJ con el resto de los partidos")
dev.off()
reglasFpvPartidos <- apriori(basketFpvPartidos , parameter=list(minlen=3, maxtime=50, support=0.35,confidence = 0.80, target = "rules"), appearance = list(lhs=fpv, default="rhs"))
inspect(sort(reglasFpvPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordFpvRestoPartidos.png" , sep="/"))
plot(head(sort(reglasFpvPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasFpvPartidos, file = paste(path_resultados_xml, "reglasFpvRestoPartidos.xml" , sep="/"))
p <- plot(reglasFpvPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueFpvRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFpvPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueFpvRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE ARGENTINA FEDERAL #####################################################
af <- c("Cordoba Federal[NEGATIVO]","Cordoba Federal[EMPATE]","Cordoba Federal[AUSENTE]","Frente de la Concordia Misionero[AUSENTE]","Frente de la Concordia Misionero[POSITIVO]","Frente de la Concordia Misionero[NEGATIVO]","Justicialista[POSITIVO]","Justicialista[NEGATIVO]","Justicialista por Tucuman[AUSENTE]","Justicialista por Tucuman[POSITIVO]","Justicialista por Tucuman[NEGATIVO]","Partido Bloquista de San Juan[NEGATIVO]","Partido Bloquista de San Juan[POSITIVO]","Partido Bloquista de San Juan[AUSENTE]","Somos San Juan[AUSENTE]","Somos San Juan[NEGATIVO]","Somos San Juan[POSITIVO]","Todos Juntos por San Juan[AUSENTE]","Todos Juntos por San Juan[NEGATIVO]","Todos Juntos por San Juan[POSITIVO]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo


#Interbloque Argentina Federal comportamiento dentro del mismo bloque
basketArgFed <- read.transactions(paste(path_archivos, "transacciones_argfed.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketArgFed)
png(filename=paste(path_resultados_graficos, "soportesInterbloquesArgentinaFederal.png" , sep="/"))
itemFrequencyPlot(basketArgFed, names = FALSE, main="Distribucion del Soporte 1-itemset para el Interbloque Argentina Federal")
dev.off()
reglasArgFed <- apriori(basketArgFed, parameter=list(minlen=7, maxtime=50, support=0.15,confidence = 0.80, target = "rules"))
gi <- generatingItemsets(reglasArgFed)
d <- which(duplicated(gi))
reglasArgFed <- reglasArgFed[-d]
inspect(sort(reglasArgFed, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordArgFed.png" , sep="/"))
plot(reglasArgFed, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasArgFed, file = paste(path_resultados_xml, "reglasInterbloqueArgFed.xml" , sep="/")) 
p <- plot(reglasArgFed, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueArgFed.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasArgFed, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200)))
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueArgFed.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketArgFedPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketArgFedPartidos)
png(filename=paste(path_resultados_graficos, "soportesArgFedPartidosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketArgFedPartidos, names = FALSE, main="Distribucion del Soporte 1-itemset para el Interbloque Argentina Federal con el resto de los partidos")
dev.off()
reglasArgFedPartidos <- apriori(basketArgFedPartidos , parameter=list(minlen=7, maxtime=50, support=0.25,confidence = 0.8, target = "rules"), appearance = list(lhs=af, default="rhs"))
#gi <- generatingItemsets(reglasArgFedPartidos)
#d <- which(duplicated(gi))
#reglasArgFedPartidos <- reglasArgFedPartidos[-d]
inspect(sort(reglasArgFedPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordArgFedRestoPartidos.png" , sep="/"))
plot(head(sort(reglasArgFedPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasArgFedPartidos, file = paste(path_resultados_xml, "reglasArgFedRestoPartidos.xml" , sep="/"))
p <- plot(reglasArgFedPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueArgFedRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasArgFedPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueArgFedRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE FRENTE RENOVADOR UNA #################################################
fr <- c("Cordoba Trabajo y Produccion[POSITIVO]","Cordoba Trabajo y Produccion[ABSTENCION]","Cordoba Trabajo y Produccion[AUSENTE]","Cordoba Trabajo y Produccion[NEGATIVO]","Cultura Educacion y Trabajo[POSITIVO]","Cultura Educacion y Trabajo[NEGATIVO]","Cultura Educacion y Trabajo[ABSTENCION]","Cultura Educacion y Trabajo[AUSENTE]","Federal Unidos por una Nueva Argentina[POSITIVO]","Federal Unidos por una Nueva Argentina[NEGATIVO]","Federal Unidos por una Nueva Argentina[ABSTENCION]","Trabajo y Dignidad[POSITIVO]","Trabajo y Dignidad[NEGATIVO]","Trabajo y Dignidad[ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque Argentina Federal comportamiento dentro del mismo bloque
basketFr <- read.transactions(paste(path_archivos, "transacciones_fruna.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketFr)
png(filename=paste(path_resultados_graficos, "soportesInterbloquesFrenteRenovador.png" , sep="/"))
itemFrequencyPlot(basketFr, names = FALSE, main="Distribucion del Soporte para 1-itemset el Interbloque Frente Renovador - UNA")
dev.off()
reglasFr <- apriori(basketFr, parameter=list(minlen=4, maxtime=50, support=0.1,confidence = 0.80, target = "rules"))
gi <- generatingItemsets(reglasFr)
d <- which(duplicated(gi))
reglasFr <- reglasFr[-d]
inspect(sort(reglasFr, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordFr.png" , sep="/"))
plot(reglasFr, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasFr, file = paste(path_resultados_xml, "reglasInterbloqueFr.xml" , sep="/"))
p <- plot(reglasArgFed, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueFr.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFr, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueFr.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketFrPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketFrPartidos)
png(filename=paste(path_resultados_graficos, "soportesFrRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketFrPartidos, names = FALSE, main="Distribucion del Soporte para 1-itemset el Interbloque Frente Renovador-UNA con el resto de los partidos")
dev.off()
reglasFrPartidos <- apriori(basketFrPartidos , parameter=list(minlen=5, maxtime=50, support=0.25,confidence = 0.80, target = "rules"), appearance = list(lhs=fr, default="rhs"))
inspect(sort(reglasFrPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordFrPartidos.png" , sep="/"))
plot(head(sort(reglasFrPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasFrPartidos, file = paste(path_resultados_xml, "reglasFrRestoPartidos.xml" , sep="/"))
p <- plot(reglasFrPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueFrRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasFrPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueFrRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################INTERBLOQUE EN MARCHA ############################################################# 
marcha <- c("Libres del Sur[AUSENTE]","Libres del Sur[POSITIVO]","Libres del Sur[NEGATIVO]","Libres del Sur[ABSTENCION]","Peronismo para la Victoria[EMPATE]","Peronismo para la Victoria[NEGATIVO]","Peronismo para la Victoria[POSITIVO]","Peronismo para la Victoria[ABSTENCION]")

#Dentro del interbloque: el objetivo es determinar las reglas de como se comportan entre los aliados. Solo se procesan los datos del interbloque mismo. El largo de las reglas es la del tamaño interbloque porque en este caso interesa ver como se comportan en bloque


#Interbloque En Marcha comportamiento dentro del mismo bloque
basketMarcha <- read.transactions(paste(path_archivos, "transacciones_enmarcha.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketMarcha)
png(filename=paste(path_resultados_graficos, "soportesInterbloquesEnMarcha.png" , sep="/"))
itemFrequencyPlot(basketMarcha, names = FALSE, main="Distribucion del Soporte para el Interbloque En Marcha")
dev.off()
reglasMarcha <- apriori(basketMarcha, parameter=list(minlen=2, maxtime=50, support=0.05,confidence = 0.80, target = "rules"))
#gi <- generatingItemsets(reglasMarcha)
#d <- which(duplicated(gi))
#reglasMarcha <- reglasMarcha[-d]
inspect(sort(reglasMarcha, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordEnMarcha.png" , sep="/"))
plot(head(sort(reglasMarcha, by = "lift"), n= 1), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasMarcha, file = paste(path_resultados_xml, "reglasInterbloqueMarcha.xml" , sep="/"))
p <- plot(reglasMarcha, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueMarcha.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasMarcha, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueMarcha.html" , sep="/"), selfcontained = FALSE)


#Interbloque contra el resto de los partidos: Del lado izquierdo de la regla se agrega las combinaciones del interbloque y se pone una longitud de interbloque + 1. La idea es obtener quien se alinea con el interbloque

basketMarchaPartidos <- read.transactions(paste(path_archivos, "transacciones_partidos.csv" , sep="/"), format = "basket", sep = ',' , rm.duplicates = FALSE)
summary(basketFrPartidos)
png(filename=paste(path_resultados_graficos, "soportesEnMarchaPartidosRestoPartidos.png" , sep="/"))
itemFrequencyPlot(basketMarchaPartidos, names = FALSE, main="Distribucion del Soporte para el Interbloque En Marcha con el resto de los partidos")
dev.off()
reglasMarchaPartidos <- apriori(basketMarchaPartidos , parameter=list(minlen=3, maxtime=50, support=0.35,confidence = 0.80, target = "rules"), appearance = list(lhs=marcha, default="rhs"))
#gi <- generatingItemsets(reglasMarchaPartidos)
#d <- which(duplicated(gi))
#reglasMarchaPartidos <- reglasMarchaPartidos[-d]
inspect(sort(reglasMarchaPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordEnMarchaPartidos.png" , sep="/"))
plot(reglasMarchaPartidos, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasMarchaPartidos, file = paste(path_resultados_xml, "reglasEnMarchaRestoPartidos.xml" , sep="/"))
p <- plot(reglasMarchaPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoInterbloqueEnMarchaRestoPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasMarchaPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizInterbloqueEnMarchaRestoPartidos.html" , sep="/"), selfcontained = FALSE)



############################################# PROVINCIAS ########################################################################

basketProvincias <- read.transactions(paste(path_archivos, "transacciones_provincias.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketProvincias)
png(filename=paste(path_resultados_graficos, "soportesProvincias.png" , sep="/"))
itemFrequencyPlot(basketProvincias, names = FALSE, main="Distribucion del Soporte de 1-itemset para las Provincias")
dev.off()
reglasProvincias <- apriori(basketProvincias, parameter=list(minlen=2, maxtime=50, support=0.50,confidence = 0.85, target = "rules"))
gi <- generatingItemsets(reglasProvincias)
d <- which(duplicated(gi))
reglasProvincias <- reglasProvincias[-d]
reglasProvincias <- reglasProvincias[is.maximal(reglasProvincias)]
inspect(sort(reglasProvincias, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordProvincias.png" , sep="/"))
plot(head(sort(reglasProvincias, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasProvincias, file = paste(path_resultados_xml, "reglasProvincias.xml" , sep="/"))
p <- plot(reglasProvincias, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoProvincias.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasProvincias, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizProvincias.html" , sep="/"), selfcontained = FALSE)
inspect(sort(reglasProvincias, by = "lift"))


########################################## PROVINCIAS y PARTIDOS ####################################################################

basketProvinciasPartidos <- read.transactions(paste(path_archivos, "transacciones_provincias_partidos.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketProvinciasPartidos)
png(filename=paste(path_resultados_graficos, "soportesProvinciasPartidos.png" , sep="/"))
itemFrequencyPlot(basketProvinciasPartidos, names = FALSE, main="Distribucion del Soporte de 1-itemset para las Provincias/ Partidos")
dev.off()
reglasProvinciasPartidos <- apriori(basketProvinciasPartidos, parameter=list(minlen=2, maxtime=50, support=0.60,confidence = 0.90, target = "rules"))
gi <- generatingItemsets(reglasProvinciasPartidos)
d <- which(duplicated(gi))
reglasProvinciasPartidos <- reglasProvinciasPartidos[-d]
reglasProvinciasPartidos <- reglasProvinciasPartidos[is.maximal(reglasProvinciasPartidos)]
inspect(sort(reglasProvinciasPartidos, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordProvinciasPartidos.png" , sep="/"))
plot(head(sort(reglasProvinciasPartidos, by = "lift"), n= 10), method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasProvinciasPartidos, file = paste(path_resultados_xml, "reglasProvinciasPartidos.xml" , sep="/"))
p <- plot(reglasProvinciasPartidos, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoProvinciasPartidos.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasProvinciasPartidos, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizProvinciasPartidos.html" , sep="/"), selfcontained = FALSE)
inspect(sort(reglasProvinciasPartidos, by = "lift"))

########################################## APROBACIONES DE LEYES ####################################################################
leyes <- c("LEY_APROBADA","LEY_RECHAZADA")

basketLeyes <- read.transactions(paste(path_archivos, "transacciones.csv" , sep="/"), format = "basket", sep = ',', rm.duplicates = FALSE)
summary(basketLeyes)
png(filename=paste(path_resultados_graficos, "soportesLeyes.png" , sep="/"))
itemFrequencyPlot(basketLeyes, names = FALSE, main="Distribucion del Soporte para las Leyes tratadas")
dev.off()
reglasLeyes <- apriori(basketLeyes, parameter=list(minlen=17, maxlen= 20, maxtime=30, support=0.4,confidence = 0.90, target = "rules"), appearance = list(default="lhs", rhs=leyes))
inspect(sort(reglasLeyes, by = "lift"))
png(filename=paste(path_resultados_graficos, "paracoordLeyes.png" , sep="/"))
plot(reglasLeyes, method = "paracoord", control = list(reorder = TRUE, col=sequential_hcl(100)))
dev.off()
write.PMML(reglasLeyes, file = paste(path_resultados_xml, "reglasLeyes.xml" , sep="/"))
p <- plot(reglasLeyes, method = "graph", engine = "htmlwidget") 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "grafoLeyes.html" , sep="/"), selfcontained = FALSE)
p <-plot(reglasLeyes, method="matrix", engine = "htmlwidget", measure=c("support","confidence"), control=list(col=sequential_hcl(200))) 
htmlwidgets::saveWidget(p, paste(path_resultados_html, "matrizLeyes.html" , sep="/"), selfcontained = FALSE)




#################################################################################################################################
#Cosas por las dudas
reglasCambiemos <- apriori(basketPartidos , parameter=list(minlen=2, maxtime=50, support=0.25,confidence = 0.75, target = "rules"), appearance = list(rhs= c("Salta Somos Todos [NEGATIVO]", "Salta Somos Todos [POSITIVO]")))
reglasCambiemos <- reglasCambiemos[is.redundant(x = reglasCambiemos, measure = "confidence")]
reglasCambiemos <- reglasCambiemos[is.maximal(reglasCambiemos)]
reglasCambiemos <- subset(reglasCambiemos, (rhs %ain% cambiemos), (lhs %ain% cambiemos))
p <- plotly_arules(basketCambiemos)
browseURL("arules.html")
