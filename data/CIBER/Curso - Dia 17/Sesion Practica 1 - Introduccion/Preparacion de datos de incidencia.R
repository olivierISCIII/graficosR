# CODIGO PARA TRASPASAR UN ARCHIVO DEL TIPO "casos.csv" A UNA ARCHIVO DEL TIPO "incidencia.csv"
###############################################################################################

# Lectura archivo casos

casos <- read.csv2("casos.csv",encoding="UTF-8")

# Cargar librerias

if(!require(reshape)) {install.packages("reshape", dep=TRUE)}
library(reshape)

# Traspaso de los casos de vejiga no invasivos que cuentan a vegiga invasivos

# Esta linea pasa las C67 de comp=1 a C67 de comp=3
casos[casos$icdo10=="D414","icdo10"] <- "C670"
# Esta linea pasa las C67 de comp=2 a C67 de comp=3
casos[casos$icdo10=="D090","icdo10"] <- "C670"

# Division del codigo ICDO10 en letra, localizacion, sublocalizacion

casos$CODCAU1 <- substr(as.character(casos$icdo10),1,1)
casos$CODCAU23 <- as.numeric(substr(as.character(casos$icdo10),2,3))
casos$CODCAU4 <- as.numeric(substr(as.character(casos$icdo10),4,4))

# Asignar codigo 999 a los missings por edad

casos[is.na(casos$edad),]$edad <- 999

names(casos)[1] <- "ZONA"
names(casos)[2] <- "AÑO"
names(casos)[3] <- "SEXO"
names(casos)[4] <- "EDAD"
names(casos)[5] <- "ICDO10"

# Selecionar solo los tumores invasivos excepto piel no melanoma

casos <- casos[casos$CODCAU1 =="C",]
casos <- casos[casos$CODCAU23 != 44,]

attach(casos)
casos$SEXO <- crea.genero()
casos$EDAD <- crea.edad()
casos$ICDO <- crea.icdo10()
detach()

aux <-c("ZONA","AÑO","SEXO","EDAD","ICDO")
casos <- casos[,aux]
rm(aux)

# Sumar el numero de casos por Zona, Año, Sexo, Edad e Icdo

n <- dim(casos)[1]
attach(casos)
incidencia <- aggregate(rep(1, n), list(ZONA=ZONA, AÑO=AÑO, SEXO=SEXO, EDAD=EDAD, ICDO=ICDO), sum)
detach()
rm(n)

# Trasponer a un archivo del tipo incidencia.csv

incidencia <- cast(incidencia, ZONA+AÑO+SEXO+ICDO~EDAD)
names(incidencia) <- c("ZONA","AÑO","SEXO","ICDO", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18","SE")
incidencia[is.na(incidencia)] <- 0 

# Funciones para preparar datos

crea.genero <- function()
{
	sexo <- as.character(SEXO)
	sexo[SEXO == 1] <- "M"
	sexo[SEXO == 2] <- "F"
	sexo
}


crea.edad <- function()
{
	edad <- trunc(EDAD/5) + 1
      edad[EDAD>=85 & EDAD<200] <- 18
	edad[EDAD==999] <- 19
      edad
}

crea.icdo10 <- function(tipo="I")
{
# Descripción: Crea un codigo icdo en funcion de una codificacion en ICD-10
	icdo <- CODCAU23
	icdo <- 99
	icdo[(CODCAU23 >= 0 & CODCAU23 <= 14)] <- 14
	icdo[CODCAU23 == 15] <- 15
	icdo[CODCAU23 == 16] <- 16
	icdo[CODCAU23 == 18] <- 18
	icdo[(CODCAU23 >= 19 & CODCAU23 <= 21)] <- 19
	icdo[CODCAU23 == 22] <- 22
	icdo[(CODCAU23 >= 23 & CODCAU23 <= 24)] <- 23
	icdo[CODCAU23 == 25] <- 25
	icdo[CODCAU23 == 32] <- 32
	icdo[(CODCAU23 >= 33 & CODCAU23 <= 34)] <- 34
	icdo[CODCAU23 == 43] <- 43
	icdo[SEXO == 2 & CODCAU23 == 50] <- 50
	icdo[SEXO == 2 & CODCAU23 == 53] <- 53
	icdo[SEXO == 2 & CODCAU23 == 54] <- 54
	icdo[SEXO == 2 & CODCAU23 == 56] <- 56
	icdo[SEXO == 1 & CODCAU23 == 61] <- 61
	icdo[SEXO == 1 & CODCAU23 == 62] <- 62
	icdo[CODCAU23 == 64] <- 64
	icdo[CODCAU23 == 67] <- 67
	icdo[(CODCAU23 >= 70 & CODCAU23 <= 72)] <- 71
	icdo[CODCAU23 == 73] <- 73
	icdo[CODCAU23 == 81] <- 81
	icdo[((CODCAU23 >= 82 & CODCAU23 <= 86) | CODCAU23==96)] <- 82
	icdo[CODCAU23 == 90] <- 90
	icdo[(CODCAU23 >= 91 & CODCAU23 <= 95)] <- 91
	icdo
}
