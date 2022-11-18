####################################################################

# Proyecto Estimaciones de Incidencia - REDECAN

####################################################################


####################################################################
#   CARGA DE DATOS
####################################################################

# Carga de librerias

if(!require(dplyr)) { install.packages("dplyr", dep=TRUE) } 
if(!require(reshape)) { install.packages("reshape", dep=TRUE) }
if(!require(coda)) { install.packages("coda", dep=TRUE) }
if(!require(R2jags)) { install.packages("R2jags", dep=TRUE) }
if(!require(rjags)) { install.packages("rjags", dep=TRUE) }

library(dplyr)
library(reshape)
library(coda)
library(R2jags)
library(rjags)

# Cargar datos de poblacion

poblacion <- read.csv2("poblacion.csv",encoding="UTF-8")

# Cargar datos de incidencia

incidencia <- read.csv2("incidencia.csv",encoding="UTF-8")

# Cargar datos mortalidad

mortalidad <- read.csv2("mortalidad.csv",encoding="UTF-8")

# Seleccionaremos 6 de las 18 provincias

incidencia.real <- incidencia
mortalidad.real <- mortalidad

set.seed(112358)
muestra <- sample(1:18,size=6,replace=FALSE)
incidencia <- incidencia %>% filter(ZONA %in% muestra)

incidencia <- incidencia %>% filter(AÑO %in% (2001:2015))
mortalidad <- mortalidad %>% filter(AÑO %in% (2000:2019))

# Cargar archivos de parametros

parametros.provincia <- read.csv2("parametros.provincia.csv",encoding="UTF-8")
parametros.tumor <- read.csv2("parametros.tumor.csv",encoding="UTF-8")
parametros.registro <- read.csv2("parametros.registro.csv",encoding="UTF-8")

# Carga Sources

source("funciones para EIR.R",encoding="UTF-8")
source("nordpred.R",encoding="UTF-8")


####################################################################
#   EJECUCION DEL PROGRAMA
####################################################################

# Creación provincia 101 (Total solo registro)

incidencia.101 <- incidencia %>% filter(ZONA %in% muestra)
attach(incidencia.101)
incidencia.global <- aggregate(incidencia.101[,5:23], list(AÑO=AÑO, SEXO=SEXO, ICDO=ICDO), sum)
detach()
n <- dim(incidencia.global)[1]
ZONA <- rep(101,n)
incidencia.global <- cbind(ZONA,incidencia.global)
incidencia <- rbind(incidencia,incidencia.global)
rm(n, ZONA, incidencia.global,incidencia.101)

mortalidad.101 <- mortalidad %>% filter(ZONA %in% muestra)
attach(mortalidad.101)
mortalidad.global <- aggregate(mortalidad.101[,5:22], list(AÑO=AÑO, SEXO=SEXO, ICDO=ICDO), sum)
detach()
n <- dim(mortalidad.global)[1]
ZONA <- rep(101,n)
mortalidad.global <- cbind(ZONA,mortalidad.global)
mortalidad <- rbind(mortalidad,mortalidad.global)
rm(n, ZONA, mortalidad.global,mortalidad.101)

poblacion.101 <- poblacion %>% filter(ZONA %in% muestra)
attach(poblacion.101)
poblacion.global <- aggregate(poblacion.101[,4:21], list(AÑO=AÑO, SEXO=SEXO), sum)
detach()
n <- dim(poblacion.global)[1]
ZONA <- rep(101,n)
poblacion.global <- cbind(ZONA,poblacion.global)
poblacion <- rbind(poblacion,poblacion.global)
rm(n, ZONA, poblacion.global,poblacion.101)

#################################
# Preparacion estimaciones 2022

# Con funcion cuartiles se determina punto de corte, p10,t2,t3,p90 

n <- nrow(parametros.tumor)
for (i in 1:n)
	{
	icdo <- parametros.tumor$CODIGO[i]
	sexo <- parametros.tumor$SEXO[i]
	if (parametros.tumor$TIPO[i]!=0) 
		{ 
		print(cuartiles(icdo,sexo)) 
		}
      }
rm(icdo,sexo,n)

# Recordar modificar archivo y recargar luego parametros.tumor
parametros.tumor <- read.csv2("parametros.tumor.csv",encoding="UTF-8")


#############################
# SIMULACION
#############################

# JAGS

sink("modelo-14-M.txt")
simulacion.icdo(14,"M","P2")
sink()
sink("modelo-15-M.txt")
simulacion.icdo(15,"M","P2")
sink()
sink("modelo-16-M.txt")
simulacion.icdo(16,"M","P2")
sink()
sink("modelo-18-M.txt")
simulacion.icdo(18,"M","P2")
sink()
sink("modelo-19-M.txt")
simulacion.icdo(19,"M","P2")
sink()
sink("modelo-22-M.txt")
simulacion.icdo(22,"M","P2")
sink()
sink("modelo-23-M.txt")
simulacion.icdo(23,"M","P2")
sink()
sink("modelo-25-M.txt")
simulacion.icdo(25,"M","P2")
sink()
sink("modelo-32-M.txt")
simulacion.icdo(32,"M","P2")
sink()
sink("modelo-34-M.txt")
simulacion.icdo(34,"M","P2")
sink()
sink("modelo-43-M.txt")
simulacion.icdo(43,"M","P2")
sink()
sink("modelo-61-M.txt")
simulacion.icdo(61,"M","P2")
sink()
sink("modelo-62-M.txt")
simulacion.icdo(62,"M","P2")
sink()
sink("modelo-64-M.txt")
simulacion.icdo(64,"M","P2")
sink()
sink("modelo-67-M.txt")
simulacion.icdo(67,"M","P2")
sink()
sink("modelo-71-M.txt")
simulacion.icdo(71,"M","P2")
sink()
sink("modelo-73-M.txt")
simulacion.icdo(73,"M","P2")
sink()
sink("modelo-81-M.txt")
simulacion.icdo(81,"M","P2")
sink()
sink("modelo-82-M.txt")
simulacion.icdo(82,"M","P2")
sink()
sink("modelo-90-M.txt")
simulacion.icdo(90,"M","P2")
sink()
sink("modelo-91-M.txt")
simulacion.icdo(91,"M","P2")
sink()
sink("modelo-99-M.txt")
simulacion.icdo(99,"M","P2")
sink()
sink("modelo-14-F.txt")
simulacion.icdo(14,"F","P2")
sink()
sink("modelo-15-F.txt")
simulacion.icdo(15,"F","P2")
sink()
sink("modelo-16-F.txt")
simulacion.icdo(16,"F","P2")
sink()
sink("modelo-18-F.txt")
simulacion.icdo(18,"F","P2")
sink()
sink("modelo-19-F.txt")
simulacion.icdo(19,"F","P2")
sink()
sink("modelo-22-F.txt")
simulacion.icdo(22,"F","P2")
sink()
sink("modelo-23-F.txt")
simulacion.icdo(23,"F","P2")
sink()
sink("modelo-25-F.txt")
simulacion.icdo(25,"F","P2")
sink()
sink("modelo-32-F.txt")
simulacion.icdo(32,"F","P2")
sink()
sink("modelo-34-F.txt")
simulacion.icdo(34,"F","P2")
sink()
sink("modelo-43-F.txt")
simulacion.icdo(43,"F","P2")
sink()
sink("modelo-50-F.txt")
simulacion.icdo(50,"F","P2",iteraciones=100000, calentamiento=60000)
sink()
sink("modelo-53-F.txt")
simulacion.icdo(53,"F","P2")
sink()
sink("modelo-54-F.txt")
simulacion.icdo(54,"F","P2")
sink()
sink("modelo-56-F.txt")
simulacion.icdo(56,"F","P2")
sink()
sink("modelo-64-F.txt")
simulacion.icdo(64,"F","P2")
sink()
sink("modelo-67-F.txt")
simulacion.icdo(67,"F","P2")
sink()
sink("modelo-71-F.txt")
simulacion.icdo(71,"F","P2")
sink()
sink("modelo-73-F.txt")
simulacion.icdo(73,"F","P2")
sink()
sink("modelo-81-F.txt")
simulacion.icdo(81,"F","P2")
sink()
sink("modelo-82-F.txt")
simulacion.icdo(82,"F","P2")
sink()
sink("modelo-90-F.txt")
simulacion.icdo(90,"F","P2")
sink()
sink("modelo-91-F.txt")
simulacion.icdo(91,"F","P2")
sink()
sink("modelo-99-F.txt")
simulacion.icdo(99,"F","P2")
sink()

tabla.rim <- extraer.tabla.rim() # Tabla con los coeficientes del modelo para cada combinacion tipo tumoral sexos


# Estimacion Mortalidad año 2022
#################################

# Preparación de datos para NORDPRED

prep.nordpred.lista(2022)
prep.nordpred.sexo(2022,"M")
prep.nordpred.sexo(2022,"F")

# Predicciones de mortalidad año 2022

aux <- c("ICDO","SEXO","AÑO","M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18")
prediccion.muertos <- mortalidad[mortalidad$ZONA==100 & mortalidad$ICDO != 100,aux]
rm(aux)
prediccion.muertos <- rbind(prediccion.muertos,lista.prediccion(2022)) # En el archivo prediccion.muertos se junta la mortalidad real (hasta 2019) junto con las estimada en 2022


# Metodo IMR
############

# Proyecciones año 2022 segun escenario

proyeccion.imr.A <- lista.proyeccion.imr(2022,"A",100)
proyeccion.imr.B <- lista.proyeccion.imr(2022,"B",100)
proyeccion.imr.C <- lista.proyeccion.imr(2022,"C",100)
proyeccion.imr.2022 <- rbind(proyeccion.imr.A,proyeccion.imr.B,proyeccion.imr.C)
rm(proyeccion.imr.A,proyeccion.imr.B,proyeccion.imr.C)


# Metodo Local Data + APC (LDA)
###############################

incidencia.0115 <- incidencia
poblacion.0115 <- poblacion

incidencia.0115 <- incidencia.0115[incidencia.0115$ZONA==101 & incidencia.0115$AÑO >= 2001 & incidencia.0115$AÑO <= 2015 & incidencia.0115$ICDO != 100,]
poblacion.0115 <- poblacion.0115[poblacion.0115$ZONA==101 & poblacion.0115$AÑO >= 2001 & poblacion.0115$AÑO <= 2015,]

# Se calculan los APC del 2006-2015

lista.trend()

# Los valores se guardan y se incorporan a parametros.tumor en los campos BETA y SDBETA. Se vuelve a cargar parametros.tumor
parametros.tumor <- read.csv2("parametros.tumor.csv",encoding="UTF-8")

lista.proyeccion.directa(101,"M",metodo="T",modo="P") 
lista.proyeccion.directa(101,"F",metodo="T",modo="P")

proyeccion.LDA.M <- lista.proyeccion.directa(101,"M",metodo="T",modo="DF") 
proyeccion.LDA.F <- lista.proyeccion.directa(101,"F",metodo="T",modo="DF")
proyeccion.LDA <- rbind(proyeccion.LDA.M,proyeccion.LDA.F)
rm(proyeccion.LDA.M,proyeccion.LDA.F)

# Contruccion archivo tam.2022
#####################################

aux <- c("CODIGO","SEXO","AÑO","METODO","TANE")
tam.2022 <- rbind(proyeccion.imr.2022[,aux],proyeccion.LDA[,aux])
rm(aux)
names(tam.2022)[5] <- "TA"
tam.2022 <- cast(tam.2022,CODIGO+SEXO+AÑO~METODO)
tam.2022 <- tam.2022[order((tam.2022$SEXO),decreasing = TRUE), ]

# IC para LDA

corrector <- as.numeric(proyeccion.LDA$N) / as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$N)
proyeccion.LDA$N_INF <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$N_INF) * corrector
proyeccion.LDA$N_SUP <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$N_SUP) * corrector
corrector <- as.numeric(proyeccion.LDA$TB) / as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TB)
proyeccion.LDA$TB_INF <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TB_INF) * corrector
proyeccion.LDA$TB_SUP <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TB_SUP) * corrector
corrector <- as.numeric(proyeccion.LDA$TAM) / as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAM)
proyeccion.LDA$TAM_INF <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAM_INF) * corrector
proyeccion.LDA$TAM_SUP <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAM_SUP) * corrector
corrector <- as.numeric(proyeccion.LDA$TAE) / as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAE)
proyeccion.LDA$TAE_INF <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAE_INF) * corrector
proyeccion.LDA$TAE_SUP <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TAE_SUP) * corrector
corrector <- as.numeric(proyeccion.LDA$TANE) / as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TANE)
proyeccion.LDA$TANE_INF <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TANE_INF) * corrector
proyeccion.LDA$TANE_SUP <- as.numeric(proyeccion.imr.2022[proyeccion.imr.2022$METODO=="A",]$TANE_SUP) * corrector
rm(corrector)

proyeccion.LDA <- proyeccion.LDA[,names(proyeccion.imr.2022)]

proyeccion.2022 <- rbind(proyeccion.imr.2022,proyeccion.LDA)


# Seleccion de escenarios
##########################

# Se usan las funciones
# grafica.rim(icdo,sexo)
# grafica.tasa.escenarios(icdo.sexo)
# Ver Documentos

# Una vez hallamos seleccionado el escenario y escrita la seleccion en parametros.tumor
# Cargaremos de nuevo el archivo parametros.tumor archivos de parametros

parametros.tumor <- read.csv2("parametros.tumor.csv",encoding="UTF-8")

# Creación de resultados
############################

proyeccion.2022 <- merge(parametros.tumor,proyeccion.2022,by.x=c("CODIGO","SEXO","ESCENARIO"),by.y=c("CODIGO","SEXO","METODO"),all.x=TRUE)
proyeccion.2022 <- proyeccion.2022[order((proyeccion.2022$SEXO),decreasing = TRUE), ]
aux <- c( "CODIGO","SEXO","VALOR","N","N_INF","N_SUP","TB","TB_INF","TB_SUP","TAM","TAM_INF","TAM_SUP","TAE","TAE_INF","TAE_SUP","TANE","TANE_INF","TANE_SUP","ESCENARIO")
proyeccion.2022 <- proyeccion.2022[,aux]
rm(aux)

proyeccion.2022.m <-proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO!=100,]
proyeccion.2022.f <-proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO!=100,] 

proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$N <- sum(as.numeric(proyeccion.2022.m$N))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$N_INF <- sum(as.numeric(proyeccion.2022.m$N)) - (sqrt(sum((((as.numeric(proyeccion.2022.m$N)) - (as.numeric(proyeccion.2022.m$N_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$N_SUP <- sum(as.numeric(proyeccion.2022.m$N)) + (sqrt(sum((((as.numeric(proyeccion.2022.m$N_SUP)) - (as.numeric(proyeccion.2022.m$N)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TB <- sum(as.numeric(proyeccion.2022.m$TB))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$Cproyeccion.2022.mODIGO==100,]$TB_INF <- sum(as.numeric(proyeccion.2022.m$TB)) - (sqrt(sum((((as.numeric(proyeccion.2022.m$TB)) - (as.numeric(proyeccion.2022.m$TB_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TB_SUP <- sum(as.numeric(proyeccion.2022.m$TB)) + (sqrt(sum((((as.numeric(proyeccion.2022.m$TB_SUP)) - (as.numeric(proyeccion.2022.m$TB)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAM <- sum(as.numeric(proyeccion.2022.m$TAM))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAM_INF <- sum(as.numeric(proyeccion.2022.m$TAM)) - (sqrt(sum((((as.numeric(proyeccion.2022.m$TAM)) - (as.numeric(proyeccion.2022.m$TAM_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAM_SUP <- sum(as.numeric(proyeccion.2022.m$TAM)) + (sqrt(sum((((as.numeric(proyeccion.2022.m$TAM_SUP)) - (as.numeric(proyeccion.2022.m$TAM)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAE <- sum(as.numeric(proyeccion.2022.m$TAE))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAE_INF <- sum(as.numeric(proyeccion.2022.m$TAE)) - (sqrt(sum((((as.numeric(proyeccion.2022.m$TAE)) - (as.numeric(proyeccion.2022.m$TAE_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TAE_SUP <- sum(as.numeric(proyeccion.2022.m$TAE)) + (sqrt(sum((((as.numeric(proyeccion.2022.m$TAE_SUP)) - (as.numeric(proyeccion.2022.m$TAE)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TANE <- sum(as.numeric(proyeccion.2022.m$TANE))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TANE_INF <- sum(as.numeric(proyeccion.2022.m$TANE)) - (sqrt(sum((((as.numeric(proyeccion.2022.m$TANE)) - (as.numeric(proyeccion.2022.m$TANE_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="M" & proyeccion.2022$CODIGO==100,]$TANE_SUP <- sum(as.numeric(proyeccion.2022.m$TANE)) + (sqrt(sum((((as.numeric(proyeccion.2022.m$TANE_SUP)) - (as.numeric(proyeccion.2022.m$TANE)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$N <- sum(as.numeric(proyeccion.2022.f$N))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$N_INF <- sum(as.numeric(proyeccion.2022.f$N)) - (sqrt(sum((((as.numeric(proyeccion.2022.f$N)) - (as.numeric(proyeccion.2022.f$N_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$N_SUP <- sum(as.numeric(proyeccion.2022.f$N)) + (sqrt(sum((((as.numeric(proyeccion.2022.f$N_SUP)) - (as.numeric(proyeccion.2022.f$N)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TB <- sum(as.numeric(proyeccion.2022.f$TB))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TB_INF <- sum(as.numeric(proyeccion.2022.f$TB)) - (sqrt(sum((((as.numeric(proyeccion.2022.f$TB)) - (as.numeric(proyeccion.2022.f$TB_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TB_SUP <- sum(as.numeric(proyeccion.2022.f$TB)) + (sqrt(sum((((as.numeric(proyeccion.2022.f$TB_SUP)) - (as.numeric(proyeccion.2022.f$TB)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAM <- sum(as.numeric(proyeccion.2022.f$TAM))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAM_INF <- sum(as.numeric(proyeccion.2022.f$TAM)) - (sqrt(sum((((as.numeric(proyeccion.2022.f$TAM)) - (as.numeric(proyeccion.2022.f$TAM_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAM_SUP <- sum(as.numeric(proyeccion.2022.f$TAM)) + (sqrt(sum((((as.numeric(proyeccion.2022.f$TAM_SUP)) - (as.numeric(proyeccion.2022.f$TAM)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAE <- sum(as.numeric(proyeccion.2022.f$TAE))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAE_INF <- sum(as.numeric(proyeccion.2022.f$TAE)) - (sqrt(sum((((as.numeric(proyeccion.2022.f$TAE)) - (as.numeric(proyeccion.2022.f$TAE_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TAE_SUP <- sum(as.numeric(proyeccion.2022.f$TAE)) + (sqrt(sum((((as.numeric(proyeccion.2022.f$TAE_SUP)) - (as.numeric(proyeccion.2022.f$TAE)))**2))))

proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TANE <- sum(as.numeric(proyeccion.2022.f$TANE))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TANE_INF <- sum(as.numeric(proyeccion.2022.f$TANE)) - (sqrt(sum((((as.numeric(proyeccion.2022.f$TANE)) - (as.numeric(proyeccion.2022.f$TANE_INF)))**2))))
proyeccion.2022[proyeccion.2022$SEXO=="F" & proyeccion.2022$CODIGO==100,]$TANE_SUP <- sum(as.numeric(proyeccion.2022.f$TANE)) + (sqrt(sum((((as.numeric(proyeccion.2022.f$TANE_SUP)) - (as.numeric(proyeccion.2022.f$TANE)))**2))))

write.csv2(proyeccion.2022,"Proyeccion 2022.csv", row.names=FALSE, fileEncoding="UTF-8")

############################################################################3





