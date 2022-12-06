####################################################################
#   CARGA DE DATOS
####################################################################

# Carga de librerias

if(!require(reshape)) {install.packages("reshape", dep=TRUE)}
library(reshape)

# Cargar datos de poblacion

# setwd("..") # Lugar donde esten ubicados los ficheros
poblacion <- read.csv2("poblacion.csv",encoding="UTF-8")

# Cargar datos de incidencia

incidencia <- read.csv2("incidencia.csv",encoding="UTF-8")

# Cargar datos mortalidad

mortalidad <- read.csv2("mortalidad.csv",encoding="UTF-8")

# Cargar archivos de parametros

parametros.registro <- read.csv2("parametros.registro.csv",encoding="UTF-8")
parametros.tumor <- read.csv2("parametros.tumor.csv",encoding="UTF-8")

# Cargar constantes

primer.año.incidencia <- 2000
ultimo.año.incidencia <- 2015
año.final.periodo <- ultimo.año.incidencia
año.inicio.periodo <- año.final.periodo - 4
año.proyeccion <- 2022

####################################################################
#   CARGA DE FUNCIONES
####################################################################


# Funciones para el cálculo de tasas

n.total <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
	a<-4+edin
	b<-4+edfn
	if (edin==1 & edfn==18 & tipo =="I") {b <- 23}	
	if(tipo=="I")
	{
            if (añoi <= añof) {
			n <- nrow(incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,])
			if (n>0 & a<b ) { casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,a:b]}
			if (n>0 & a==b ) { casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,a]}
			if(n==0)  { casos <- 0 }
			}
		if(añoi > añof) {casos <- 0}
	}
	if(tipo=="M")
	{
            if (añoi <= añof) {
               	n <- nrow(mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,])
			if (n>0 & a<b ) {casos <- mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,a:b]}
			if (n>0 & a==b ) {casos <- mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,a]}
			if(n==0)  { casos <- 0 }
			}
		if(añoi > añof) {casos <- 0}
	}
	sum(casos)

}

n.año <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
	años <- (añof-añoi)+1
	n.total(tipo, zona, icdo, sexo, añoi, añof, edin, edfn)/años
}

tb <- function(tipo="I", zona=100,icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
	a<-3+edin
	b<-3+edfn
      if (añoi <= añof) {	
		casos <- n.total(tipo, zona, icdo, sexo, añoi, añof, edin, edfn)
		pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,a:b]
		tb <- casos/sum(pob)*100000
	}
	if(añoi > añof) {tb <- 0}
	if(sexo=="A" & parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] == 0 )
	{
		tb <- tb(tipo,zona,icdo,"F",añoi,añof,edin,edfn)
	}
	if(sexo=="A" & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] == 0 )
	{
		tb <- tb(tipo,zona,icdo,"M",añoi,añof,edin,edfn)
	}
	tb
}

tee <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
	pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,4:21]
	aux <- rep(0,dim(pob)[1])
	pob <- aggregate(pob,list(aux=aux),sum)
	pob <- pob[2:19]
	rm(aux) 
	if(tipo=="I")
	{
            n <- nrow(incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,5:23]) 
		if (n>0) {casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,5:23]}
            if (n==0) {casos <- t(rep(0,18))} 	
	}
	if(tipo=="M")
	{
		n <- nrow(mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,5:22])
		if (n>0) {casos <- mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,5:22]}	
            if (n==0) {casos <- t(rep(0,18))} 
	}
	aux <- rep(0,dim(casos)[1])
	mis <- 0
	casos <- aggregate(casos,list(aux=aux),sum)
	if (n>0 & tipo=="I") {mis <- casos[,20]}
	casos <- casos[,2:19]
	rm(aux) 
	tasa <- (casos/pob)*100000
	tot <- n.total(tipo, zona, icdo, sexo, añoi, añof, 1, 18)	
	if (tot>0) {
		tasa <- tasa * (tot/(tot - mis))
	}
	tasa 
}

ta <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18, estandard=1)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
     	if (añoi <= añof)
	{
		ta <- 0
		edades <- rep(0,18)
		edades[edin:edfn] <- 1
		if(estandard == 1) # Nueva Piramide Europea
		{
			w <- matrix(c(0.05, 0.055, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07, 0.07, 0.07, 0.065, 0.06, 0.055, 0.05, 0.04, 0.025, 0.025),nrow = 18, ncol = 1)
		}
		if(estandard == 2) # Piramide Mundial
		{
			w <- matrix(c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005),nrow = 18, ncol = 1)
		}
		if(estandard == 3) # Vieja Piramide europea
		{
			w <- matrix(c(0.08, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.01),nrow = 18, ncol = 1)
		}
		k <- sum(w[edin:edfn])
		w <- edades * w * (1/k)
		if((sexo=="M" | sexo=="A") & parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1)
		{
			tee <- tee(tipo, zona, icdo, "M", añoi, añof)
			ta.m <- as.matrix(tee) %*% w
			if (sexo=="M")
			{
				ta <- ta.m
			}
		}
		if((sexo=="F" | sexo=="A") & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1)
		{
			tee <- tee(tipo, zona, icdo, "F", añoi, añof)
			ta.f <- as.matrix(tee) %*% w
			if (sexo=="F")
			{
				ta <- ta.f
			}
		}
		if(sexo=="A")
		{
			if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
			{
	      		ta <- (ta.m + ta.f)/2
	 		}
			if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] == 0) 
			{
	      		ta <- ta.m 
	 		}
			if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] == 0 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
			{
	      		ta <- ta.f
	 		}
		}
	}
	if(añoi > añof) {ta <- 0}
	ta
}

indicadores.tumor <- function(tipo="I", zona=100, icdo, sexo, añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18)
{
	valor.icdo <- parametros.tumor[parametros.tumor$CODIGO==icdo, "VALOR"]
	valor.n <- n.año(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)	
	valor.tb <- tb(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)
	valor.tae <- ta(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, 1)
	valor.tam <- ta(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, 2)
	cat(format(valor.icdo,width=50),fmt.n(valor.n,8,0),fmt.n(valor.tb,6,1),fmt.n(valor.tae,6,1),fmt.n(valor.tam,6,1),"\n")
}

lista.incidencia <- function(tipo="I", zona=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo, edin=1, edfn=18)
{
	añoi2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOI"]
	añof2 <- parametros.registro[parametros.registro$ZONA == zona,"AÑOF"]
	if(añoi < añoi2) {añoi <- añoi2}
	if(añof > añof2) {añof <- añof2}
	cat("","\n")
	n <- nrow(parametros.tumor)
	provincia <- as.character(parametros.registro[parametros.registro$ZONA==zona,"DESCRIPCION"])
	cat(provincia," (",fmt.n(añoi, 4, 0)," - ", fmt.n(añof, 4, 0),")","\n")
	cat("---------------------------------------------","\n")
	cat("","\n")
	if(sexo=="M") {
		cat("HOMBRES:","\n")
		}
	if(sexo=="F") {
		cat("MUJERES:","\n")
		}
	if(sexo=="A") {
		cat("AMBOS SEXOS:","\n")
		}
	cat("","\n")
	cat("TIPO TUMORAL                                          N/AÑO    TB    TAe    TAm ","\n")
	cat("----------------------------------------------------- ----- ------ ------ ------","\n")
      salida <- c("")
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(sexo=="M" & parametros.tumor$M[i] != "0")
		{
			salida <- rbind(salida,indicadores.tumor(tipo,zona,icdo,sexo,añoi,añof,edin,edfn))
	
		}
		if(sexo=="F" & parametros.tumor$F[i] != "0")
		{
			salida <- rbind(salida,indicadores.tumor(tipo,zona,icdo,sexo,añoi,añof,edin,edfn))
	
		}
		if(sexo=="A" & parametros.tumor$A[i] != "0")
		{
			salida <- rbind(salida,indicadores.tumor(tipo,zona,icdo,sexo,añoi,añof,edin,edfn))
	
		}
	}
      salida <- salida[2:dim(salida)[1]]
	salida
	cat("","\n")
}


grafica.edad <- function(tipo="I", zona=100, icdo, sexo, añoi=año.inicio.periodo, añof=año.final.periodo)
{
	edad <- rep(1:18)
	edad2 <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
	tee <- tee("I", zona, icdo, sexo, añoi, añof)
	tee <- t(as.matrix(tee))
	maxy <- max(tee)
	if (tipo=="I")
	{
		if(sexo=="M")	{   plot(edad,tee,type="o", main="Gráfica Incidencia por edad en hombres", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=12, xaxt="n", ylim = c(0,maxy)) }
		if(sexo=="F") 	{   plot(edad,tee,type="o", main="Gráfica Incidencia por edad en mujeres", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=14, xaxt="n", ylim = c(0,maxy)) }
		if(sexo=="A") 	{   plot(edad,tee,type="o", main="Gráfica Incidencia por edad en ambos sexos", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=14, xaxt="n", ylim = c(0,maxy)) }
	}
	if (tipo=="M")
	{
		if(sexo=="M")	{   plot(edad,tee,type="o", main="Gráfica Mortalidad por edad en hombres", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=12, xaxt="n", ylim = c(0,maxy)) }
		if(sexo=="F") 	{   plot(edad,tee,type="o", main="Gráfica Mortalidad por edad en mujeres", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=14, xaxt="n", ylim = c(0,maxy)) }
		if(sexo=="A") 	{   plot(edad,tee,type="o", main="Gráfica Mortalidad por edad en ambos sexos", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=14, xaxt="n", ylim = c(0,maxy)) }
	}
	axis(side=1, edad, tcl=-0.2, labels=FALSE, las=3,)
	mtext(edad2, side=1, las=1, at=edad, line=0.3, cex=0.9, las=3, font=2,)
}


# Funciones para el calculo de tendencias

datos.apc <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.final.periodo-9, añof=año.final.periodo)
{
	if (tipo=="I") {
		casos <- incidencia[incidencia$ZONA == zona & incidencia$ICDO == icdo & incidencia$SEXO== sexo & incidencia$AÑO >=  añoi & incidencia$AÑO <= añof,]
		casos$TOT <- rowSums(casos[,5:23])
		casos$p <- (casos$TOT-casos$SE)/casos$TOT
	}
	if (tipo=="M") {
		casos <- mortalidad[mortalidad$ZONA == zona & mortalidad$ICDO == icdo & mortalidad$SEXO== sexo & mortalidad$AÑO >=  añoi & mortalidad$AÑO <= añof,]
		casos$TOT <- rowSums(casos[,5:22])
		casos$p <- (casos$TOT)/casos$TOT
	}
	pobs <- poblacion[poblacion$ZONA == zona & poblacion$SEXO == sexo & poblacion$AÑO >=  añoi & poblacion$AÑO <= añof,]
	datos <- merge(casos,pobs,by=c("ZONA","SEXO","AÑO"))
	datos$P01 <- datos$P01*datos$p
	datos$P02 <- datos$P02*datos$p
	datos$P03 <- datos$P03*datos$p
	datos$P04 <- datos$P04*datos$p
	datos$P05 <- datos$P05*datos$p
	datos$P06 <- datos$P06*datos$p
	datos$P07 <- datos$P07*datos$p
	datos$P08 <- datos$P08*datos$p
	datos$P09 <- datos$P09*datos$p
	datos$P10 <- datos$P10*datos$p
	datos$P11 <- datos$P11*datos$p
	datos$P12 <- datos$P12*datos$p
	datos$P13 <- datos$P13*datos$p
	datos$P14 <- datos$P14*datos$p
	datos$P15 <- datos$P15*datos$p
	datos$P16 <- datos$P16*datos$p
	datos$P17 <- datos$P17*datos$p
	datos$P18 <- datos$P18*datos$p
	if(tipo=="I"){	aux <- c("AÑO","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10","I11","I12","I13","I14","I15","I16","I17","I18")}
	if(tipo=="M"){	aux <- c("AÑO","M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11","M12","M13","M14","M15","M16","M17","M18")}
	casos <- datos[,aux]
	aux <- c("AÑO","P01","P02","P03","P04","P05","P06","P07","P08","P09","P10","P11","P12","P13","P14","P15","P16","P17","P18")
	pobs <- datos[,aux]
	aux <- c("AÑO","G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12","G13","G14","G15","G16","G17","G18")
	names(casos) <- aux
	names(pobs) <- aux
	casos <- melt(casos,id=c("AÑO"))
	pobs <- melt(pobs,id=c("AÑO"))
	datos <- merge(casos,pobs,by=c("AÑO","variable"))
	aux <- c("AÑO","EDAD","X","POB")
	names(datos) <- aux
	datos
}

apc <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.final.periodo-9, añof=año.final.periodo)
{
	datos <- datos.apc(tipo, zona, icdo, sexo, añoi, añof)
	global <- glm(X ~ AÑO + EDAD + offset(log(POB)), family = poisson, data = datos)
	coef <- global$coe[2]
	apc <- (100 * (exp(coef)- 1))
	apc
}

trend <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.final.periodo-9, añof=año.final.periodo, tit = NULL)
{
	if (sexo=="M" | sexo=="F")
	{
		datos <- datos.apc(tipo, zona, icdo, sexo, añoi, añof)
	}
	if (sexo=="A" & parametros.tumor[parametros.tumor$CODIGO==icdo,]$F==0)
	{
		datos <- datos.apc(tipo, zona, icdo, "M", añoi, añof)
	}
	if (sexo=="A" & parametros.tumor[parametros.tumor$CODIGO==icdo,]$M==0)
	{
		datos <- datos.apc(tipo, zona, icdo, "F", añoi, añof)
	}
	if (sexo=="A" & parametros.tumor[parametros.tumor$CODIGO==icdo,]$M>=1 & parametros.tumor[parametros.tumor$CODIGO==icdo,]$F>=1)
	{
		datos <- datos.apc(tipo, zona, icdo, sexo, añoi, añof)
	}	
	global <- glm(X ~ AÑO + EDAD + offset(log(POB)), family = poisson, data = datos)
	if(is.null(tit)) {
		tit <- parametros.tumor[parametros.tumor$CODIGO==icdo, "VALOR"]
	}
      if(sexo=="M") {tit.sx <- " - Hombres"}
      if(sexo=="F") {tit.sx <- " - Mujeres"}
      if(sexo=="A") {tit.sx <- " - Ambos Sexos"}
	tit <- paste(c(as.character(tit), tit.sx), collapse = "", sep = " ")
	n <- dim(datos)[1]
	uno <- rep(1, n)
	s <- list(global = global, n.global = tapply(datos$X, uno, sum), tit = tit, call = match.call())
	class(s) <- "trend"
	s
}

print.trend <- function(x, ...)
{
	rr(x$tit, x$global, x$n.global)
}

rr <- function(tit, o, nn)
{
	e <- summary(o)$coe
	dnam <- dimnames(e)[[1]]
	pos <- grep("*AÑO*", dnam)
	e <- e[pos,  ]
	n <- dim(e)[1]
	if(is.null(n)) {
		n <- 1
		dim(e) <- c(1, 4)
		dimnames(e) <- list(dnam[pos], names(e))
	}
	for(i in 1:n) {
		cat(format(tit, width=50), fmt.n(nn, 8, 0), fmt(100 * (exp(e[i, 1]) - 1), 6, 1), " (", fmt(100 * (exp(e[i, 1] - 1.96 * e[i, 2]) - 1), 6, 1), "-",fmt(100 * (exp(e[i, 1] + 1.96 * e[i, 2]) - 1), 6, 1), ")", ifelse(abs(e[i, 3]) > 1.96, "*", " "), "\n")
	}
}

lista.tendencia <- function(tipo="I", zona=100, sexo="M", uaño=año.final.periodo, puntos=10)
{
  	añoi <- (uaño - puntos + 1)
  	añof <- uaño
	n <- nrow(parametros.tumor)
	provincia <- as.character(parametros.registro[parametros.registro$ZONA==zona,"DESCRIPCION"])
	cat("","\n")
	cat(provincia," (",fmt.n(añoi, 4, 0)," - ", fmt.n(añof, 4, 0),")", "\n")
	cat("------------------------------","\n")
	cat("","\n")
	if(sexo=="M") {
		cat("HOMBRES:","\n")
		}
	if(sexo=="F") {
		cat("MUJERES:","\n")
		}
	if(sexo=="A") {
		cat("AMBOS SEXOS:","\n")
		}
	cat("---------","\n")
	cat("","\n")
	cat("-----------------------------------------------------------------------------------------\n")
  	cat("TIPO TUMORAL:                                             N   %incr         I.C. 95%\n")
  	cat("-----------------------------------------------------------------------------------------\n")
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(sexo=="M" & parametros.tumor$M[i] != "0")
		{
			print(trend(tipo,zona,icdo,sexo,añoi,añof))
	
		}
		if(sexo=="F" & parametros.tumor$F[i] != "0")
		{
			print(trend(tipo,zona,icdo,sexo,añoi,añof))
	
		}
		if(sexo=="A" & parametros.tumor$A[i] != "0")
		{
			print(trend(tipo,zona,icdo,sexo,añoi,añof))
	
		}
	}
	cat("","\n")
}

# Funciones para el calculo de proyecciones

tasa.uaño  <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=año.inicio.periodo, añof=año.final.periodo)
{
	datos <- datos.apc(tipo, zona, icdo, sexo, añoi, añof)
	global <- glm(X ~ AÑO + EDAD + offset(log(POB)), family = poisson, data = datos)
	coef <- global$coe
	tee <- rep(0,18)
	tee[1] <- exp(coef[1]+(añof*coef[2]))
	for (i in 2:18) {
		tee[i] <- exp(coef[1]+(añof*coef[2])+coef[i+1])
		}
	tee <- t(tee)*100000
	tee
}

proyeccion.tasas <- function(tipo="I", zona=100, icdo=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	añoit <- añof - (puntos-1)
	if(metodo=="T") {
		añoi <- (añof-base+1)
		añob <- (añoi + añof)/2
		tee <- tee(tipo,zona,icdo,sexo,añoi,añof)
		}
	if(metodo=="L") {
		añoi <- añoit
		añob <- añof
		tee <- tasa.uaño(tipo,zona,icdo,sexo,añoi,añof)
	}
	if(tipo=="I")
	{
		apc <- apc(tipo,zona,icdo,sexo,añoit,añof)
		if (r != 0) {
			apc <- max(-r,min(r,apc))
		}
		b <- 1+(apc/100)
		if(m != 0) {
			a <- (apc/100) * (1/m)
		}
		if(m == 0) {
			a <- 0
		}
	}
	if(tipo=="M")
	{
		apcm <- apc(tipo,zona,icdo,sexo,añoit,añof)
		apc <- apc("I",zona,icdo,sexo,añoit,añof)
		if (r != 0) {
			apcm <- min(max(-r,min(r,apcm)),apc)
		}
		if (r == 0) {
			apcm <- min(apcm,apc)
		}
		b <- 1+(apcm/100)
		if(m != 0) {
			a <- (apcm/100) * (1/m)
		}
		if(m == 0) {
			a <- 0
		}	
	}
	for(año in (añob + 1):añop) 
	{
		tee <- tee * b
		if(a>0)
		{
			b <- max(1, b-a)
		}
		if(a<0)
		{
			b <- min(1, b-a)
		}
	}
	tee
}

proyeccion.n <- function(tipo="I", zona=100, icdo=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	if(sexo=="M" | sexo=="F")
	{
		if (icdo != 100)
		{
			edades <- rep(0,18)
			edades[edin:edfn] <- 1
			tee <- proyeccion.tasas(tipo, zona, icdo, sexo, añof, añop, metodo, base, puntos, r, m) * edades
			pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]
			n <- as.matrix(tee) %*% as.matrix(t(pob))
			n <- n/100000
		}
		if (icdo == 100) 
		{
			n <- 0
			k <- nrow(parametros.tumor)
			for (i in 1:k) 
			{
				tumor <- parametros.tumor$CODIGO[i]
				if(sexo=="M" & parametros.tumor$M[i] == 1) 
				{
		      		n <- n + proyeccion.n(tipo,zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m) 
		 		}
				if(sexo=="F" & parametros.tumor$F[i] == 1) 
				{
		         		n <- n + proyeccion.n(tipo,zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m) 
		 		}
			}
		}
	}
	if(sexo=="A")
	{
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	n <- proyeccion.n(tipo,zona,icdo,"M",añof,añop,edin,edfn,metodo,base,puntos,r,m) + proyeccion.n(tipo,zona,icdo,"F",añof,añop,edin,edfn,metodo,base,puntos,r,m)
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] == 0) 
		{
	      	n <- proyeccion.n(tipo,zona,icdo,"M",añof,añop,edin,edfn,metodo,base,puntos,r,m)
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] == 0 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	n <- proyeccion.n(tipo,zona,icdo,"F",añof,añop,edin,edfn,metodo,base,puntos,r,m)
	 	}
	}
	n
}


proyeccion.tb <- function(tipo="I", zona=100, icdo=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	if(sexo=="M" | sexo=="F")
	{
		if (icdo != 100)
		{
			edades <- rep(0:18)
			edades[edin:edfn] <- 1
			n <- proyeccion.n(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m)
			pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]
			pob <- pob * edades
			pob <- sum(pob)
			tb <- (n/pob)*100000
		}
		if (icdo == 100) 
		{
			tb <- 0
			k <- nrow(parametros.tumor)
			for (i in 1:k) 
			{
				tumor <- parametros.tumor$CODIGO[i]
				if(sexo=="M" & parametros.tumor$M[i] == 1) 
				{
		         		tb <- tb + proyeccion.tb(tipo,zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m) 
		 		}
				if(sexo=="F" & parametros.tumor$F[i] == 1) 
				{
		         		tb <- tb + proyeccion.tb(tipo,zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m) 
		 		}
			}
		}
	}
	if(sexo=="A")
	{
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	tb <- (proyeccion.tb(tipo,zona,icdo,"M",añof,añop,edin,edfn,metodo,base,puntos,r,m) + proyeccion.tb(tipo,zona,icdo,"F",añof,añop,edin,edfn,metodo,base,puntos,r,m))/2
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] == 0) 
		{
	      	tb <- proyeccion.tb(tipo,zona,icdo,"M",añof,añop,edin,edfn,metodo,base,puntos,r,m)
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] == 0 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	tb <- proyeccion.tb(tipo,zona,icdo,"F",añof,añop,edin,edfn,metodo,base,puntos,r,m)
	 	}
	}
	tb
}

proyeccion.ta <- function(tipo="I", zona=100, icdo=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, edin=1, edfn=18, estandard=1, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	if(sexo=="M" | sexo=="F")
	{
		if (icdo != 100)
		{
			edades <- rep(0,18)
			edades[edin:edfn] <- 1
			if(estandard == 1)  # Nueva Piramide europea
			{
				w <- matrix(c(0.05, 0.055, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07, 0.07, 0.07, 0.065, 0.06, 0.055, 0.05, 0.04, 0.025, 0.025),nrow = 18, ncol = 1)
			}
			if(estandard == 2) # Piramide Mundial
			{
				w <- matrix(c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005),nrow = 18, ncol = 1)
			}
			if(estandard == 3) # Vieja Piramide europea
			{
				w <- matrix(c(0.08, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.01),nrow = 18, ncol = 1)
			}
			k <- sum(w[edin:edfn])
			w <- edades * w * (1/k)
			tee <- proyeccion.tasas(tipo, zona, icdo, sexo, añof, añop, metodo, base, puntos, r, m) * edades
			ta <- as.matrix(tee) %*% w
		}
		if (icdo == 100) 
		{
			ta <- 0
			k <- nrow(parametros.tumor)
			for (i in 1:k) 
			{
			tumor <- parametros.tumor$CODIGO[i]
			if(sexo=="M" & parametros.tumor$M[i] == 1) 
				{
		      		ta <- ta + proyeccion.ta(tipo,zona,tumor,sexo,añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m) 
		 		}
			if(sexo=="F" & parametros.tumor$F[i] == 1) 
				{
		      		ta <- ta + proyeccion.ta(tipo,zona,tumor,sexo,añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m) 
		 		}
			}
		}
	}
	if(sexo=="A")
	{
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	ta <- (proyeccion.ta(tipo,zona,icdo,"M",añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m) + proyeccion.ta(tipo,zona,icdo,"F",añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m))/2
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] == 0) 
		{
	      	ta <- proyeccion.ta(tipo,zona,icdo,"M",añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m)
	 	}
		if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] == 0 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
		{
	      	ta <- proyeccion.ta(tipo,zona,icdo,"F",añof,añop,edin,edfn,estandard,metodo,base,puntos,r,m)
	 	}
	}
	ta
}


indicadores.proyeccion <- function(tipo="I", zona=100, icdo=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	valor.icdo <- parametros.tumor[parametros.tumor$CODIGO==icdo, "VALOR"]
	valor.n <- proyeccion.n(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m)
	valor.tb <- proyeccion.tb(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m)
	valor.tae <- proyeccion.ta(tipo,zona,icdo,sexo,añof,añop,edin,edfn,1,metodo,base,puntos,r,m)
	valor.tam <- proyeccion.ta(tipo,zona,icdo,sexo,añof,añop,edin,edfn,2,metodo,base,puntos,r,m)
	cat(format(valor.icdo,width=50),fmt.n(valor.n,8,0),fmt.n(valor.tb,6,1),fmt.n(valor.tae,6,1),fmt.n(valor.tam,6,1),"\n")
}

lista.proyeccion <- function(tipo="I", zona=100, sexo="M", añof=año.final.periodo, añop=año.proyeccion, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	cat("","\n")
	n <- nrow(parametros.tumor)
	provincia <- as.character(parametros.registro[parametros.registro$ZONA==zona,"DESCRIPCION"])
	cat(provincia," (",fmt.n(añop, 4, 0),")","\n")
	cat("------------------------------","\n")
	cat("","\n")
	if(sexo=="M") {
		cat("HOMBRES:","\n")
		}
	if(sexo=="F") {
		cat("MUJERES:","\n")
		}
	if(sexo=="A") {
		cat("AMBOS SEXOS:","\n")
		}
	cat("","\n")
	cat("TIPO TUMORAL                                             N     TB    TAe    TAm ","\n")
	cat("----------------------------------------------------- ----- ------ ------ ------","\n")
      salida <- c("")
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(sexo=="M" & parametros.tumor$M[i] != "0")
		{
			salida <- rbind(salida,indicadores.proyeccion(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m))
	
		}
		if(sexo=="F" & parametros.tumor$F[i] != "0")
		{
			salida <- rbind(salida,indicadores.proyeccion(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m))
	
		}
		if(sexo=="A" & parametros.tumor$A[i] != "0")
		{
			salida <- rbind(salida,indicadores.proyeccion(tipo,zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,puntos,r,m))
	
		}
	}
      salida[2:dim(salida)[1]]
	salida
	cat("","\n")
}

evolucion.indicadores.año <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=primer.año.incidencia, añof=año.proyeccion, uaño=ultimo.año.incidencia, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	valor.icdo <- as.character(parametros.tumor[parametros.tumor$CODIGO==icdo, "VALOR"])
	cat("","\n")
	cat(valor.icdo," (",fmt.n(añoi, 4, 0)," - ", fmt.n(añof, 4, 0),")", "\n")
	cat("-----------------------------------","\n")
	cat("AÑO:        N     TB    TAe    TAm ","\n")
	cat("----- -------- ------ ------ ------","\n")
	for(año in añoi:añof)
	{
		if (año<=uaño)
		{
			valor.n <- n.año(tipo,zona,icdo, sexo, año, año, edin, edfn)	
			valor.tb <- tb(tipo,zona,icdo, sexo, año, año, edin, edfn)
			valor.tae <- ta(tipo,zona, icdo, sexo, año, año, edin, edfn, 1)
			valor.tam <- ta(tipo,zona, icdo, sexo, año, año, edin, edfn, 2)
			cat(fmt.n(año,5,0),fmt.n(valor.n,8,0),fmt.n(valor.tb,6,1),fmt.n(valor.tae,6,1),fmt.n(valor.tam,6,1),"\n")
		}
		if (año>uaño)
		{
			valor.n <- proyeccion.n(tipo,zona,icdo,sexo,uaño,año,edin,edfn,metodo,base,puntos,r,m)
			valor.tb <- proyeccion.tb(tipo,zona,icdo,sexo,uaño,año,edin,edfn,metodo,base,puntos,r,m)
			valor.tae <- proyeccion.ta(tipo,zona,icdo,sexo,uaño,año,edin,edfn,1,metodo,base,puntos,r,m)
			valor.tam <- proyeccion.ta(tipo,zona,icdo,sexo,uaño,año,edin,edfn,2,metodo,base,puntos,r,m)
			cat(fmt.n(año,5,0),fmt.n(valor.n,8,0),fmt.n(valor.tb,6,1),fmt.n(valor.tae,6,1),fmt.n(valor.tam,6,1),"\n")
		}
	}
}


grafica.n.año <- function(tipo="I",zona=100, icdo, sexo, añoi=primer.año.incidencia, añof=año.proyeccion, uaño=ultimo.año.incidencia, edin=1, edfn=18, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	n <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		if (i <= uaño) {
			n[i-(añoi-1)] <- n.año(tipo, zona, icdo, sexo, i, i, edin, edfn)
		}
		if (i > uaño) {
			n[i-(añoi-1)] <- proyeccion.n(tipo,zona,icdo,sexo,uaño,i,edin,edfn,metodo,base,puntos,r,m)
		}
		if (n[i-(añoi-1)] > maxy) 
		{
			maxy <- n[i-(añoi-1)]
		}
	}
	if (tipo=="I")
	{
		if(sexo=="M")	{plot(año,n,type="o", main="Evolución del número de casos incidentes en hombres", xlab="AÑO", ylab="N", las=2, font=2, tck=1, lwd=2, col=12,xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")   {plot(año,n,type="o", main="Evolución del número de casos incidentes en mujeres", xlab="AÑO", ylab="N", las=2, font=2, tck=1, lwd=2, col=14,xaxt="n", ylim = c(0,maxy))}
	}
	if (tipo=="M")
	{
		if(sexo=="M")	{plot(año,n,type="o", main="Evolución del número de casos de mortalidad en hombres", xlab="AÑO", ylab="N", las=2, font=2, tck=1, lwd=2, col=12,xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")   {plot(año,n,type="o", main="Evolución del número de casos de mortalidad en mujeres", xlab="AÑO", ylab="N", las=2, font=2, tck=1, lwd=2, col=14,xaxt="n", ylim = c(0,maxy))}
	}
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3,)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3, font=2,)
}


grafica.ta.año <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi=primer.año.incidencia, añof=año.proyeccion, uaño=ultimo.año.incidencia, edin=1, edfn=18, estandard=1, metodo="T", base = 5, puntos = 10, r = 3, m = 20)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	ta <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		if (i <= uaño) {
			ta[i-(añoi-1)] <- ta(tipo, zona, icdo, sexo, i, i, edin, edfn, estandard)
		}
		if (i > uaño) {
			ta[i-(añoi-1)] <- proyeccion.ta(tipo,zona,icdo,sexo,uaño,i,edin,edfn,estandard,metodo,base,puntos,r,m)
		}
		if (ta[i-(añoi-1)] > maxy)
		{
			maxy <- ta[i-(añoi-1)]
		}
	}
	if (tipo=="I")
	{
		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de incidencia en hombres", xlab="AÑO", ylab="TA", las=2, font=2, tck=1, lwd=2, col=12,xaxt="n", ylim = c(0,maxy))}	
		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de incidencia en mujeres", xlab="AÑO", ylab="TA", las=2, font=2, tck=1, lwd=2, col=14,xaxt="n", ylim = c(0,maxy))}
	}
	if(tipo=="M")
	{
		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de mortalidad en hombres", xlab="AÑO", ylab="TA", las=2, font=2, tck=1, lwd=2, col=12,xaxt="n", ylim = c(0,maxy))}	
		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de mortalidad en mujeres", xlab="AÑO", ylab="TA", las=2, font=2, tck=1, lwd=2, col=14,xaxt="n", ylim = c(0,maxy))}
	}	
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3, font=2,)
}


# Otras funciones

fmt <- function(n, len, d)
{
	a <- format(round(n, d), digits = len, nsmall = d)
	for(i in 1:length(a)) {
		if(nchar(a[i]) > len)
		stop("No se puede")
		else if(len - nchar(a[i]) > 0)
		a[i] <- paste(c(rep(" ", len - nchar(a[i])), a[i]), collapse = "")
	}
a
}

fmt.n <- function(n, l, d)
{
	a <- format(round(n, d))
	if(l == nchar(a))
	a
	else {
		if(l - nchar(a) < 1)
		stop("No se puede")
		paste(c(rep(" ", l - nchar(a)), a), collapse = "")
	}
}



