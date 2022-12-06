# Carga de librerias

if(!require(reshape)) {install.packages("reshape", dep=TRUE)}
library(reshape)

# CODIGO GENERADOR DE SIMULACION DE DATOS
#########################################

# GENERACION DE DATOS SIMULADOS DE POBLACION
###############################################

set.seed(112358)
zona <- c(1:18)
sexo <- c("M","F")
datos <- expand.grid(ZONA=zona,SEXO=sexo)

# Se genera una población inicial en cada provincia de entre 100000 y 1000000

X <- runif(18)
NT <- 10**(5+X)
datos.nt <- data.frame(ZONA=zona,NT=NT)
datos <- merge(datos,datos.nt,by=c("ZONA"),all.x=TRUE)
rm(X,NT,datos.nt)

# Se genera un punto de canvio en las tendencias del punto de cambio entre 2005 y 2020 

datos$pt.change <- 2005 + runif(36)*15
año <- c(2000:2025)
datos.año <- expand.grid(ZONA=zona,SEXO=sexo,AÑO=año)
datos <- merge(datos.año,datos,by=c("ZONA","SEXO"),all.x=TRUE)
rm(datos.año)

# Se generan dos tendencias diferentes para cada segmento
# -Una tendencia entre el 1% y 3% anual desde 2000 al punto de cambio 
# -Una tendencia entre el -1% y 1% anual desde el punto de cambio al 2025

for(l in 1:18)
	{
	for(j in 2001:2025)
		{
		if(j < datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==2000,]$pt.change)
			{
			datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==j,]$NT <- (datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==(j-1),]$NT) * (1.01 + (runif(1)*0.02) )
			}
		if(j > datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==2000,]$pt.change)
			{
			datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==j,]$NT <- (datos[datos$ZONA==l & datos$SEXO=="M" & datos$AÑO==(j-1),]$NT) * (0.99 + (runif(1)*0.02) )
			}
		if(j < datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==2000,]$pt.change)
			{
			datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==j,]$NT <- (datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==(j-1),]$NT) * (1.01 + (runif(1)*0.02) )
			}
		if(j > datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==2000,]$pt.change)
			{
			datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==j,]$NT <- (datos[datos$ZONA==l & datos$SEXO=="F" & datos$AÑO==(j-1),]$NT) * (0.99 + (runif(1)*0.02) )
			}
		}
	}
rm(l,j)

edad <- c(1:18)
datos.aux <- expand.grid(ZONA=zona,SEXO=sexo,AÑO=año,EDAD=edad)
datos <- merge(datos.aux,datos,by=c("ZONA","SEXO","AÑO"),all.x=TRUE)
rm(datos.aux)

#  Generar una distribución de de los 18 grupos de edad como una mixtura de p1 y p2
#      p1=Distribucion de los 18 grupos de edad en España en 1971
#      p2=Distribucion de los 18 grupos de edad en España en 2050
#      q1=Distribucion de probabilidad del año 2000 cercana a p1
#      q2=Distribucion de probabilidad del año 2025 cercana a p2

datos.piramide <-  read.csv2("piramide.csv",encoding="UTF-8")

datos.p <- expand.grid(ZONA=zona,SEXO=sexo)
datos.p$q1 <- runif(36,0.5,1)
datos.p$q2 <- runif(36,0,0.5)

datos.aux <- expand.grid(ZONA=zona,SEXO=sexo,EDAD=edad)
datos.piramide <- merge(datos.aux,datos.piramide,by=c("EDAD","SEXO"),all.x=TRUE)
datos.piramide <- merge(datos.piramide,datos.p,by=c("ZONA","SEXO"),all.x=TRUE)
datos <- merge(datos,datos.piramide,by=c("ZONA","SEXO","EDAD"),all.x=TRUE)
datos$j <- datos$AÑO - 2000
datos$q <- datos$q1 + ((datos$q2-datos$q1)*((1/25)*datos$j))
datos$p <- datos$q*datos$p1 + (1-datos$q)*datos$p2
datos$n <- datos$NT * datos$p 
datos$POB <- rep(0,nrow(datos))
for (i in 1:nrow(datos))
	{
    datos$POB[i] <- rpois(1,datos$n[i])
	}
rm(datos.piramide,datos.p,datos.aux,i)
datos <- datos[,c("ZONA","SEXO","EDAD","AÑO","POB")]
datos <- datos[order(datos$ZONA, (as.numeric(datos$SEXO)), datos$AÑO, datos$EDAD), ]
poblacion <- datos


# GENERACION DE DATOS SIMULADOS DE INCIDENCIA
###############################################

lista.tumores <- read.csv2("lista.tumores.csv",encoding="UTF-8")
incidencia.2025 <-  read.csv2("incidencia.2025.csv",encoding="UTF-8")
trend.Spain <-  read.csv2("trend.Spain.csv",encoding="UTF-8")

# A partir de las tasas de incidencia por edad de las estimaciones del 2022 se genera un modelo que servira para generar tasas de incidencia por edad del 2025 

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M",AÑO=año)
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F",AÑO=año)
datos.aux <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)

datos.aux <- merge(datos.aux,incidencia.2025,by=c("ICDO","SEXO"),all.x=TRUE)

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M")
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F")
datos.trend <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)


# Se genera un punto de canvio en las tendencias del punto de cambio entre 2010 y 2015 
# Se generan dos tendencias diferentes para cada segmento
# -Una tendencia desde 2000 al punto de cambio similar al apc anual entre 2002 y 2012 
# -Una tendencia desde el punto de cambio al 2025 similar al apc anual entre 2012 y 2022

datos.trend$pt.change <- 2010 + runif(nrow(datos.trend))*5
datos.trend <- merge(datos.trend,trend.Spain,by=c("ICDO","SEXO"),all.x=TRUE)
datos.aux <- merge(datos.aux,datos.trend,by=c("ZONA","ICDO","SEXO"),all.x=TRUE)
rm(datos.trend)

datos.aux$apc1 <- 1 + (datos.aux$apc1/100)
datos.aux$apc2 <- 1 + (datos.aux$apc2/100)
datos.aux$apc <- rep(1,nrow(datos.aux))

# Para hombres

icdo <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
for(m in icdo)
	{
	for(l in 1:18)
		{
		for(j in 2024:2000)
			{
			if(j > datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==2025,]$pt.change)
				{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc2)
				}
			if(j < datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==2025,]$pt.change)
				{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc1)
				}
			}
		}
	}
rm(icdo,m,l,j)

# Para mujeres

icdo <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
for(m in icdo)
	{
	for(l in 1:18)
		{
		for(j in 2024:2000)
			{
			if(j > datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==2025,]$pt.change)
				{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc2)
				}
			if(j < datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==2025,]$pt.change)
				{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc1)
				}
			}
		}
	}
rm(icdo,m,l,j)

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M",AÑO=año,EDAD=edad)
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F",AÑO=año,EDAD=edad)
datos.trend <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)
datos.trend <- merge(datos.trend,datos.aux,by=c("ZONA","ICDO","SEXO","AÑO"),all.x=TRUE)
rm(datos.aux)

datos.trend$age <- (datos.trend$EDAD * 5) - 2.5
datos.trend$tee <- ((datos.trend$a*datos.trend$age)^(datos.trend$k))
datos.trend$tee <- datos.trend$tee * datos.trend$apc

# Se creara un efecto Provincia en las tasas de incidencia

efecto.zona <- exp(rnorm(18,0.05,0.05))
efecto.zona <- data.frame(ZONA=zona,efecto.pr=efecto.zona)

# Se creara un efecto interacion Provincia-tumor en las tasas de incidencia

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M")
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F")
efecto.tumor.zona <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)
efecto.tumor.zona <- merge(efecto.tumor.zona,efecto.zona,by=c("ZONA"),all.x=TRUE)
efecto.tumor.zona$ef.tum.pr <- exp(rnorm(nrow(efecto.tumor.zona),0.05,0.05))

datos.trend <- merge(datos.trend,efecto.tumor.zona,by=c("ZONA","ICDO","SEXO"),all.x=TRUE)

datos.trend$tee <- datos.trend$tee * datos.trend$apc * datos.trend$efecto.pr * datos.trend$ef.tum.pr

datos <- merge(datos.trend,datos,by=c("ZONA","SEXO","AÑO","EDAD"),all.x=TRUE)
rm(datos.trend)

# Multiplicaremos las tasas de incidencia por la poblacion simulada

datos$n <- datos$POB * datos$tee 
datos$CASOS <- rep(0,nrow(datos))
for (i in 1:nrow(datos))
	{
    datos$CASOS[i] <- rpois(1,datos$n[i])
	}

datos <- datos[,c("ICDO","ZONA","SEXO","EDAD","AÑO","POB","CASOS")]
datos <- datos[order(datos$ICDO,datos$ZONA, (as.numeric(datos$SEXO)), datos$AÑO, datos$EDAD), ]
incidencia <- datos[,c("ICDO","ZONA","SEXO","EDAD","AÑO","CASOS")]


# GENERACION DE DATOS SIMULADOS DE MORTALIDAD
###############################################

# A partir de las tasas de mortalidad por edad de las estimaciones del 2015 se genera un modelo que servira para generar tasas de mortalidad por edad del 2015 


mortalidad.2015 <-  read.csv2("mortalidad.2015.csv",encoding="UTF-8")

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M",AÑO=año)
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F",AÑO=año)
datos.aux <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)

datos.aux <- merge(datos.aux,mortalidad.2015,by=c("ICDO","SEXO"),all.x=TRUE)

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M")
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F")
datos.trend <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)


# Se generan dos tendencias de mortalidad para cada segmento
# -Una tendencia desde 2000 a 2015 similar al apc anual de incidencia entre 2002 y 2012 restandole entre un 0.5 y un 1.5 
# -Una tendencia desde 2015 a 2025 similar al apc anual entre 2012 y 2022 restandole entre un 0.5 y un 1.5

datos.trend <- merge(datos.trend,trend.Spain,by=c("ICDO","SEXO"),all.x=TRUE)
datos.trend$mod1 <- runif(nrow(datos.trend),-1.5,-0.5)
datos.trend$mod2 <- runif(nrow(datos.trend),-1.5,-0.5)
datos.trend$apc1 <- datos.trend$apc1 + datos.trend$mod1
datos.trend$apc2 <- datos.trend$apc2 + datos.trend$mod2

datos.aux <- merge(datos.aux,datos.trend,by=c("ZONA","ICDO","SEXO"),all.x=TRUE)
rm(datos.trend)

datos.aux$apc1 <- 1 + ((datos.aux$apc1 + datos.aux$mod1) / 100)
datos.aux$apc2 <- 1 + ((datos.aux$apc2 + datos.aux$mod2) / 100)
datos.aux$apc <- rep(1,nrow(datos.aux))

# Para hombres

icdo <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
for(m in icdo)
	{
	for(l in 1:18)
		{
		for(j in 2014:2000)
			{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc1)
			}
		for(j in 2016:2005)
			{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==(j-1),]$apc) * (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="M" & datos.aux$AÑO==j,]$apc2)
			}
		}
	}
rm(icdo,m,l,j)

# Para mujeres

icdo <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
for(m in icdo)
	{
	for(l in 1:18)
		{
		for(j in 2014:2000)
			{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==(j+1),]$apc) / (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc1)
			}
		for(j in 2016:2005)
			{
				datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc <- (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==(j-1),]$apc) * (datos.aux[datos.aux$ICDO==m & datos.aux$ZONA==l & datos.aux$SEXO=="F" & datos.aux$AÑO==j,]$apc2)
			}
		}
	}
rm(icdo,m,l,j)

icdo.h <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="M",c("ICDO")]
datos.aux.h <- expand.grid(ICDO=icdo.h,ZONA=zona,SEXO="M",AÑO=año,EDAD=edad)
icdo.f <- lista.tumores[lista.tumores$CALCULO==1 & lista.tumores$SEXO=="F",c("ICDO")]
datos.aux.f <- expand.grid(ICDO=icdo.f,ZONA=zona,SEXO="F",AÑO=año,EDAD=edad)
datos.trend <- rbind(datos.aux.h,datos.aux.f)
rm(icdo.h,icdo.f,datos.aux.h,datos.aux.f)
datos.trend <- merge(datos.trend,datos.aux,by=c("ZONA","ICDO","SEXO","AÑO"),all.x=TRUE)
rm(datos.aux)

datos.trend$age <- (datos.trend$EDAD * 5) - 2.5
datos.trend$tee <- ((datos.trend$a*datos.trend$age)^(datos.trend$k))
datos.trend$tee <- datos.trend$tee * datos.trend$apc

# Se aplican los mismos efectos provincia y efecto Tumor-Provincia aplicados en la generacion de incidencia

datos.trend <- merge(datos.trend,efecto.tumor.zona,by=c("ZONA","ICDO","SEXO"),all.x=TRUE)
rm(efecto.zona,efecto.tumor.zona)

datos.trend$tee <- datos.trend$tee * datos.trend$apc * datos.trend$efecto.pr * datos.trend$ef.tum.pr

datos <- merge(datos.trend,datos,by=c("ICDO","ZONA","SEXO","AÑO","EDAD"),all.x=TRUE)
rm(datos.trend)

# Multiplicaremos las tasas de mortalidad por la poblacion simulada

datos$n <- datos$POB * datos$tee 
datos$DEF <- rep(0,nrow(datos))
for (i in 1:nrow(datos))
	{
    datos$DEF[i] <- rpois(1,datos$n[i])
	}

datos <- datos[,c("ICDO","ZONA","SEXO","EDAD","AÑO","POB","CASOS","DEF")]
datos <- datos[order(datos$ICDO,datos$ZONA, (as.numeric(datos$SEXO)), datos$AÑO, datos$EDAD), ]
mortalidad <- datos[,c("ICDO","ZONA","SEXO","EDAD","AÑO","DEF")]

casos <- incidencia
rm(datos)


# TRASPASO DEL FORMATO "casos.csv" AL FORMATO "incidencia.csv"
###############################################################


incidencia <- cast(incidencia, ZONA+AÑO+SEXO+ICDO~EDAD)
incidencia$SE <-rep(0,nrow(incidencia)) # Solo si no existieran casos sin edad
names(incidencia) <- c("ZONA","AÑO","SEXO","ICDO", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18","SE")
incidencia[is.na(incidencia)] <- 0 

mortalidad <- cast(mortalidad, ZONA+AÑO+SEXO+ICDO~EDAD)
names(mortalidad) <- c("ZONA","AÑO","SEXO","ICDO", "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18")
mortalidad[is.na(mortalidad)] <- 0 

poblacion <- cast(poblacion, ZONA+AÑO+SEXO~EDAD)
names(poblacion) <- c("ZONA","AÑO","SEXO","P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18")
poblacion[is.na(poblacion)] <- 0 


# CREACION DE CODIGOS TODAS NO PIEL, AMBOS SEXOS Y SUMA DE TODAS LAS PROVINCIAS
###############################################################################


# Creacion de "Todas no piel melanoma" (codigo=100)

ICDO<-rep(100,dim(incidencia)[1])
incidencia.global <- aggregate(incidencia[5:23],list(ZONA=incidencia$ZONA,ICDO=ICDO,SEXO=incidencia$SEXO,AÑO=incidencia$AÑO),sum)
incidencia <-rbind(incidencia,incidencia.global)
rm(ICDO,incidencia.global)

ICDO<-rep(100,dim(mortalidad)[1])
mortalidad.global <- aggregate(mortalidad[5:22],list(ZONA=mortalidad$ZONA,ICDO=ICDO,SEXO=mortalidad$SEXO,AÑO=mortalidad$AÑO),sum)
mortalidad <-rbind(mortalidad,mortalidad.global)
rm(ICDO,mortalidad.global)

# Creacion variable ambos sexos ("A") para incidencia, mortalidad i poblacion

SEXO<-rep("A",dim(incidencia)[1])
incidencia.global <- aggregate(incidencia[5:23],list(ZONA=incidencia$ZONA,ICDO=incidencia$ICDO,SEXO=SEXO,AÑO=incidencia$AÑO),sum)
incidencia <-rbind(incidencia,incidencia.global)
rm(SEXO,incidencia.global)

SEXO<-rep("A",dim(mortalidad)[1])
mortalidad.global <- aggregate(mortalidad[5:22],list(ZONA=mortalidad$ZONA,ICDO=mortalidad$ICDO,SEXO=SEXO,AÑO=mortalidad$AÑO),sum)
mortalidad <-rbind(mortalidad,mortalidad.global)
rm(SEXO,mortalidad.global)

SEXO<-rep("A",dim(poblacion)[1])
poblacion.global <- aggregate(poblacion[4:21],list(ZONA=poblacion$ZONA,SEXO=SEXO,AÑO=poblacion$AÑO),sum)
poblacion <-rbind(poblacion,poblacion.global)
rm(SEXO,poblacion.global)

# Creacion de zona sumatorio (codigo=100)

ZONA<-rep(100,dim(incidencia)[1])
incidencia.global <- aggregate(incidencia[5:23],list(ZONA=ZONA,ICDO=incidencia$ICDO,SEXO=incidencia$SEXO,AÑO=incidencia$AÑO),sum)
incidencia <-rbind(incidencia,incidencia.global)
rm(ZONA,incidencia.global)

ZONA<-rep(100,dim(mortalidad)[1])
mortalidad.global <- aggregate(mortalidad[5:22],list(ZONA=ZONA,ICDO=mortalidad$ICDO,SEXO=mortalidad$SEXO,AÑO=mortalidad$AÑO),sum)
mortalidad <-rbind(mortalidad,mortalidad.global)
rm(ZONA,mortalidad.global)

ZONA<-rep(100,dim(poblacion)[1])
poblacion.global <- aggregate(poblacion[4:21],list(ZONA=ZONA,SEXO=poblacion$SEXO,AÑO=poblacion$AÑO),sum)
poblacion <-rbind(poblacion,poblacion.global)
rm(ZONA,poblacion.global)

# Exportacion ficheros incidencia, mortalidad, poblacion y casos

write.csv2(incidencia,"incidencia.csv", row.names=FALSE, fileEncoding="UTF-8")
write.csv2(mortalidad,"mortalidad.csv", row.names=FALSE, fileEncoding="UTF-8")
write.csv2(poblacion,"poblacion.csv", row.names=FALSE, fileEncoding="UTF-8")






