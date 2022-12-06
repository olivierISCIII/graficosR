
# FUNCIONES EXPLOTACION DE DATOS 2001-2015
###########################################

n.total <- function(tipo="I", zona=101,icdo=100, sexo="M", añoi, añof, edin=1, edfn=18)
{
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

n.año <- function(tipo="I",zona=101,icdo=100, sexo="M", añoi, añof, edin=1, edfn=18)
{
	años <- (añof-añoi)+1
	n.total(tipo, zona, icdo, sexo, añoi, añof, edin, edfn)/años
}

tb <- function(tipo="I", zona=101,icdo=100, sexo="M", añoi, añof, edin=1, edfn=18)
{
	if(tipo == "I") {
		añoi2 <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOI"]
		añof2 <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOF"]
		if(añoi < añoi2) {añoi <- añoi2}
		if(añof > añof2) {añof <- añof2}
	}
        if (añoi <= añof) {	
		a<-3+edin
		b<-3+edfn
		casos <- n.total(tipo, zona, icdo, sexo, añoi, añof, edin, edfn)
		pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,a:b]
		tb <- casos/sum(pob)*100000
	}
	if(añoi > añof) {tb <- 0}
	tb
}

tee <- function(tipo="I", zona=101, icdo=100, sexo="M", añoi, añof)
{
	pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,4:21]
	aux <- rep(0,dim(pob)[1])
	pob <- aggregate(pob,list(aux=aux),sum)
	pob <- pob[2:19]
	rm(aux) 
	if(tipo=="I") 	{
		n <- nrow(incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,5:23])
		if (n>0) {casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXO==sexo & incidencia$AÑO>=añoi & incidencia$AÑO<=añof,5:23]}
		if (n==0) {casos <- t(rep(0,19))} 
	}
	if(tipo=="M") {
		n <- nrow(mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,5:22])
		if (n>0) {casos <- mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO>=añoi & mortalidad$AÑO<=añof,5:22]}
		if (n==0) {casos <- t(rep(0,18))} 
	}
	aux <- rep(0,dim(casos)[1])
	casos <- aggregate(casos,list(aux=aux),sum)
	rm(aux)
	if (tipo == "I") {
		mis <- casos[,20]
		casos <- casos[,2:19]
		tasa <- (casos/pob)*100000
		tot <- n.total(tipo, zona, icdo, sexo, añoi, añof, 1, 18)
		tasa <- tasa * (tot/(tot - mis))
	}
	if (tipo == "M") {
		casos <- casos[,2:19]
		tasa <- (casos/pob)*100000
	}
	tasa 
}


ta <- function(tipo="I",zona=101, icdo=100, sexo="M", añoi, añof, edin=1, edfn=18, estandard=1)
{
	if(tipo == "I") {
		añoi2 <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOI"]
		añof2 <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOF"]
		if(añoi < añoi2) {añoi <- añoi2}
		if(añof > añof2) {añof <- añof2}
	}
        if (añoi <= añof) {
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
		tee <- tee(tipo, zona, icdo, sexo, añoi, añof)
		ta <- as.matrix(tee) %*% w
	}
	if(añoi > añof) {ta <- 0}
	ta
}

indicadores.tumor <- function(tipo="I", zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
	valoricdo <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo, "VALOR"]
	valorn <- n.año(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)	
	valortb <- tb(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)
	valorta <- ta(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, estandard)
	aux <- c(as.character(valoricdo),round(valorn,1),round(valortb,1),round(valorta,1))
	aux
}

indicadores.provincia <- function(tipo="I", zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
	valorprov <- parametros.provincia[parametros.provincia$CODIGO==zona, "VALOR"]
	valorn <- n.total(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)
	valortb <- tb(tipo,zona,icdo, sexo, añoi, añof, edin, edfn)
	valorta <- ta(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, estandard)
	aux <- c(as.character(valorprov),round(valorn,0),round(valortb,1),round(valorta,1))
	aux
}

evolucion.indicadores.año <- function(tipo="I", zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
	salida <- c("AÑO:","N","TB","TA")
	print(salida)
	for(año in añoi:añof)
	{
		n <- n.año(tipo,zona,icdo, sexo, año, año, edin, edfn)	
		tb <- tb(tipo,zona,icdo, sexo, año, año, edin, edfn)
		ta <- ta(tipo,zona, icdo, sexo, año, año, edin, edfn, estandard)
		salida <- c(format.default(año,0),round(n,0),round(tb,1),round(ta,1))
		print(salida)
	}
}

lista.provincia <- function(tipo="I",icdo, sexo, añoi, añof, edin=1, edfn=18, estandard=1)
{
	salida <- c("ZONA:", "N.TOTAL", "TB", "TA")
	if (tipo == "I") {
		n <- dim(parametros.registro)[1]
		for (i in 1:n) {
			zona <- parametros.registro$REGISTRO[i]
			salida <- rbind(salida, indicadores.provincia(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, estandard))
                	}
		}
	if (tipo == "M") {
		for (zona in 1:18) {
			salida <- rbind(salida, indicadores.provincia(tipo,zona, icdo, sexo, añoi, añof, edin, edfn, estandard))
 			}
		salida <- rbind(salida, indicadores.provincia(tipo,100, icdo, sexo, añoi, añof, edin, edfn, estandard))
		salida <- rbind(salida, indicadores.provincia(tipo,101, icdo, sexo, añoi, añof, edin, edfn, estandard))
		}
	salida
}
		
lista.tumor <- function(tipo="I", zona=101, sexo, añoi=2011, añof=2015, edin=1, edfn=18, estandard=1)
{
	# Retorna n.año, tasa bruta y tasa ajustada para un listado de tumores.
	salida <- c("TUMOR:", "N/AÑO", "TB", "TA")
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(parametros.tumor$TIPO[i] >= 1 & parametros.tumor$SEXO[i]==sexo)
		salida <- rbind(salida,indicadores.tumor(tipo,zona,icdo,sexo,añoi,añof,edin,edfn,estandard))
	}
	salida
}

# Graficas

grafica.ta <- function(tipo="I", zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	ta <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		ta[i-(añoi-1)] <- ta(tipo, zona, icdo, sexo, i, i, edin, edfn, estandard)
		if (ta[i-(añoi-1)] > maxy)
		{
			maxy <- ta[i-(añoi-1)]
		}
	}
	if(sexo=="M" | zona!= 100){plot(año,ta,type="o", main=toupper(parametros.registro[parametros.registro$REGISTRO==zona,"DESCRIPCION"]), col=12,xaxt="n", ylim = c(0,maxy))}
	if(sexo=="M" | zona== 100){plot(año,ta,type="o", main=toupper("ESPAÑA"), col=12,xaxt="n", ylim = c(0,maxy))}
	if(sexo=="F" | zona!= 100){plot(año,ta,type="o", main=toupper(parametros.registro[parametros.registro$REGISTRO==zona,"DESCRIPCION"]), col=14,xaxt="n", ylim = c(0,maxy))}
	if(sexo=="F" | zona== 100){plot(año,ta,type="o", main=toupper("ESPAÑA"), col=14,xaxt="n", ylim = c(0,maxy))}
#	if (tipo=="I")
#	{
#		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de incidencia en hombres", col=12,xaxt="n", ylim = c(0,maxy))}	
#		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de incidencia en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
#	}
#	if(tipo=="M")
#	{
#		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de mortalidad en hombres", col=12,xaxt="n", ylim = c(0,maxy))}	
#		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada de mortalidad en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
#	}	
       axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
       mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3)
}

puntos.ta <- function(tipo="I", zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
       naños <- (añof-añoi)+1
       año <- rep(añoi:añof)
       ta <- rep(1:naños, 0)
       maxy <- 0
       for(i in añoi:añof)
       {
       		ta[i-(añoi-1)] <- ta(tipo, zona, icdo, sexo, i, i, edin, edfn, estandard)
       }
       salida <- cbind(año,ta)
       salida
}


grafica.n.año <- function(tipo="I",zona=101, icdo, sexo, añoi=2001, añof=2015, edin=1, edfn=18, estandard=1)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	n <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		n[i-(añoi-1)] <- n.año(tipo,zona,icdo,sexo,i,i,1,18)
		if (n[i-(añoi-1)] > maxy) 
		{
			maxy <- n[i-(añoi-1)]
		}
	}
	if (tipo=="I")
	{
		if(sexo=="M")	{plot(año,n,type="o", main="Evolución del número de casos incidentes en hombres", col=12,xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")   {plot(año,n,type="o", main="Evolución del número de casos incidentes en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
	}
	if (tipo=="M")
	{
		if(sexo=="M")	{plot(año,n,type="o", main="Evolución del número de casos de mortalidad en hombres", col=12,xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")   {plot(año,n,type="o", main="Evolución del número de casos de mortalidad en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
	}
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3)
}


# Evolución 2001-2015
# Calculo Idicadores: n, ta		

n.estimada <- function (zona=100, icdo, sexo, año)
{
	if (icdo != 100) {
		n <- sum(n.estimada.edad(zona,icdo,sexo, año))
	}
	n
}

n.estimada.edad <- function (zona=100, icdo, sexo, año)
{
	muertos <- mortalidad[mortalidad$ZONA==zona & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo & mortalidad$AÑO==año,5:22]
	rim <- rim.estimada(zona, icdo, sexo, año)
	n <- muertos * rim
	n
}

tee.estimada <- function (zona=100, icdo, sexo, añoi, añof)
{
if (icdo != 100) {
    pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof ,4:21]
    aux <- rep(0,dim(pob)[1])
    pob <- aggregate(pob,list(aux=aux),sum)
    pob <- pob[2:19]
    rm(aux)
    casos <- rep(0,18)
    for (año in añoi:añof) {
        casos <- casos + n.estimada.edad(zona,icdo,sexo,año)
        }
    tee <- (casos/pob)*100000
    }
if (icdo == 100) {
    tee <- rep(0,18)
    n <- nrow(parametros.tumor)
    for(i in 1:n)
    {
        if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i]==sexo)
        {
            tumor <- parametros.tumor$CODIGO[i]
            tee <- tee + tee.estimada(zona, tumor, sexo, añoi, añof)
            }
         }
    }
tee
}

ta.estimada <- function (zona=100, icdo, sexo, añoi, añof ,estandard=1)
{
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
    tee <- tee.estimada(zona, icdo, sexo, añoi, añof)
    ta <- as.matrix(tee) %*% w
    ta
}


tb.estimada <- function (zona=100, icdo, sexo, año)
{
	if (icdo != 100) 
	{
		pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==año,4:21]
		casos <- n.estimada.edad(zona,icdo,sexo,año)
		casos <- sum(casos)
		pob <- sum(pob)
		tb <- (casos/pob) * 100000
	}
	tb
}


tasaacum.estimada <- function(zona=100, icdo, sexo, edad, añoi, añof)
{
	gedad <- edad/5
	taac <- 0
	tee <- tee.estimada(zona, icdo, sexo, añoi, añof)/100000
	for(i in 1:gedad)
	{
    		taac <- taac + (as.numeric(tee[i])*5)
	}
	taac <- taac*100
	taac
}

risc <- function(zona=100, icdo, sexo, edad, añoi=2011, añof=2015)
{
	taac <- tasaacum.estimada(zona, icdo, sexo, edad, añoi, añof)/100
	risc <- (1-exp(-taac))*100
	risc
} 


evolucion.indicadores.estimados.año <- function(zona=100,icdo,sexo,añoi=2001,añof=2015)
{
	salida <- c("AÑO:","N","TB","TAm", "TAe", "TAne")
	print(salida)
	for(año in añoi:añof)
	{
		n <- n.estimada(zona, icdo, sexo, año)	
		tb <- tb.estimada(zona, icdo, sexo, año)
		tam <- ta.estimada(zona, icdo, sexo, año, año, 2)
		tae <- ta.estimada(zona, icdo, sexo, año, año, 3)
		tane <- ta.estimada(zona, icdo, sexo, año, año, 1)
		salida <- c(format.default(año,0),round(n,0),round(tb,1),round(tam,1), round(tae,1),round(tane,1))
		print(salida)
	}
}

lista.evolucion.indicadores.estimados.año <- function(zona=100, añoi=2001, añof=2015)
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		sexo <- parametros.tumor$SEXO[i]
		if(parametros.tumor$TIPO[i] != "0")
		{
			if(i==1)
			{
				salida <- evolucion.indicadores.estimados.año(zona,icdo,sexo,añoi, añof)
			}
			if(i!=1)
			{
				salida <- rbind(salida,evolucion.indicadores.estimados.año(zona,icdo,sexo,añoi, añof))
			}	
		}
	}
	salida
}

indicadores.tumor.ta.estimada <- function(zona=100, icdo, sexo, añoi=2001, añof=2015, estandard=1)
{
	valoricdo <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo, "VALOR"]
	valorta <- ta.estimada(zona, icdo, sexo, añoi, añof, estandard)
	aux <- c(as.character(valoricdo),round(valorta,1))
	aux
}

lista.tumor.ta.estimada <- function(zona=100, sexo, añoi=2011, añof=2015, estandard=1)
{
	# Retorna n.año, tasa bruta y tasa ajustada para un listado de tumores.
	salida <- c("TUMOR:", "TA")
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i] == sexo)
		{
			salida <- rbind(salida,evolucion.indicadores.estimados.año(zona,icdo,sexo,añoi, añof))
		}
	}
	salida
}

rim.estimada <- function (zona=100, icdo, sexo, año)
{
 	q1 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P10"]
	q2 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T2"]
	q3 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T3"]
	q4 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P90"] 
      rim <- rep(0,18)
	coeficientes <- tabla.rim[tabla.rim$ICDO == icdo & tabla.rim$SEXO == sexo,]
	if (zona == 1) { coe.a <- as.numeric(coeficientes$alpha.1.) }
	if (zona == 4) { coe.a <- as.numeric(coeficientes$alpha.2.) }
	if (zona == 6) { coe.a <- as.numeric(coeficientes$alpha.3.) }
	if (zona == 10) { coe.a <- as.numeric(coeficientes$alpha.4.) }
	if (zona == 12) { coe.a <- as.numeric(coeficientes$alpha.5.) }
	if (zona == 13) { coe.a <- as.numeric(coeficientes$alpha.6.) }
	if (zona == 100) { coe.a <- as.numeric(coeficientes$a) }
	coe.año <- (año-2001)*as.numeric(coeficientes$YEAR) + ((año-2001)*(año-2001))*as.numeric(coeficientes$YEAR2)
	for (i in 1:18){
		edad <- (i*5)-2.5
		if (i==18) { edad <- 90}
		coe.edad1 <- as.numeric(coeficientes$EDAD1)*pmax(0,edad-q1)
		coe.edad2 <- as.numeric(coeficientes$EDAD2)*pmax(0,edad-q2)
		coe.edad3 <- as.numeric(coeficientes$EDAD3)*pmax(0,edad-q3)
		coe.edad4 <- as.numeric(coeficientes$EDAD4)*pmax(0,edad-q4)
		coe.edad <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
		rim[i] <- exp(coe.a+coe.año+coe.edad)
	}
	rim
}

evolucion.rim <- function(icdo, sexo, escenario="C", añoi=2001, añof=2015, edad=13)
{
      naños <- (añof - añoi) + 1
	ndatos <- naños*2
	aux <- matrix(rep(0,ndatos),ncol=2,nrow=naños)
	for(año in añoi:añof) {
		aux[año-2000,1] <- año
		if (año <= 2015) {
			aux[año-2000,2] <- rim.estimada(100,icdo,sexo,año)[edad]
			}
		if (año > 2015) {
			if (escenario=="A") {
				aux[año-2000,2] <- rim.estimada(100,icdo,sexo,2015)[edad]
			}
			if (escenario=="B") {
				rim.2001 <- rim.estimada(100,icdo,sexo,2001)[edad]
				rim.2015 <- rim.estimada(100,icdo,sexo,2015)[edad]
				u.2001 <- log(rim.2001)
				u.2015 <- log(rim.2015)         
				u.proyectado <- u.2015 + ((u.2015-u.2001)/(2015-2001)) * (año-2015)                        
				rim.proyectado <- exp(u.proyectado)
				aux[año-2000,2] <- rim.proyectado
			}
			if (escenario=="C") {
				aux[año-2000,2] <- rim.estimada(100,icdo,sexo,año)[edad]
			}
		}
	}
	aux
}

# graficas 2001-2015

grafica.ta.estimada <- function(zona=100, icdo, sexo, añoi=2001, añof=2015, estandard=1)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	ta <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		ta[i-(añoi-1)] <- ta.estimada(zona, icdo, sexo, i, i, estandard)
		if (ta[i-(añoi-1)] > maxy)
		{
			maxy <- ta[i-(añoi-1)]
		}
	}
	if (estandard==1) 
	{
		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la nueva tasa ajustada europea en hombres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la nueva tasa ajustada europea en mujeres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
	}
	if (estandard==2)
	{
		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada mundial en hombres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada mundial en mujeres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
	}
	if (estandard==3) 
	{
		if(sexo=="M")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada europea en hombres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
		if(sexo=="F")	{plot(año,ta,type="o", main="Evolución de la tasa ajustada europea en mujeres", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAm", tck=1, col=4, lwd=2, xaxt="n", ylim = c(0,maxy))}
	}
axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
mtext(año, side=1, las=1, at=año, line=0.3, cex=1, las=3, font=2)
}



grafica.incmort.ta <- function(zona=100, icdo, añoi=2001, añof=2015, estandard=1)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	tai <- rep(1:naños, 0)
	tam <- rep(1:naños,0)
	maxy <- 0
	for(i in añoi:añof)
	{
		tai[i-(añoi-1)] <- ta.estimada(zona, icdo, sexo, i, i, estandard)
		tam[i-(añoi-1)] <- ta("M",zona, icdo, sexo, i, i, 1,18, estandard)
		if (tai[i-(añoi-1)] > maxy)
		{
			maxy <- tai[i-(añoi-1)]
		}
	}
	if (estandard==1) 	
	{
		plot(año,tai,type="o", main="Evolución de la nueva tasa ajustada europea", col=3,xaxt="n", ylim = c(0,maxy))
		par(new=TRUE)
		plot(año,tam,type="o", col=6, xaxt="n", ylim = c(0,maxy), axes=FALSE) 
	}
	if (estandard==2) 	
	{
		plot(año,tai,type="o", main="Evolución de la tasa ajustada mundial", col=3,xaxt="n", ylim = c(0,maxy))
		par(new=TRUE)
		plot(año,tam,type="o", col=6, xaxt="n", ylim = c(0,maxy), axes=FALSE) 
	}
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3)
}

grafica.n.estimada <- function(zona=100, icdo, sexo, añoi=2001, añof=2015)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	n <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		n[i-(añoi-1)] <- n.estimada(zona=100, icdo, i)
		if (n[i-(añoi-1)] > maxy) 
		{
			maxy <- n[i-(añoi-1)]
		}
	}
	if(sexo=="M")	{plot(año,n,type="o", main="Evolución del número de casos incidentes en hombres", col=11,xaxt="n", ylim = c(0,maxy))}
	if(sexo=="F")	{plot(año,n,type="o", main="Evolución del número de casos incidentes en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
	
axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3)
}

grafica.tb.estimada <- function(zona=100, icdo, sexo, añoi=2001, añof=2015)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	tb <- rep(1:naños, 0)
	maxy <- 0
	for(i in añoi:añof)
	{
		tb[i-(añoi-1)] <- tb.estimada(zona, icdo, i)
		if (tb[i-(añoi-1)] > maxy)
		{
			maxy <- tb[i-(añoi-1)]
		}
	}
	if(sexo=="M")	{plot(año,tb,type="o", main="Evolución de la tasa bruta de incidencia en hombres", col=11,xaxt="n", ylim = c(0,maxy))}
	if(sexo=="F")	{plot(año,tb,type="o", main="Evolución de la tasa bruta de incidencia en mujeres", col=14,xaxt="n", ylim = c(0,maxy))}
		
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=0.9, las=3)
}

# Graficas Estimacion 2001-2015 y Proyecciones 2022

grafica.tasa.escenarios <- function(icdo, sexo, añoi=2001, uañoi=2015, uañom = 2019, añof=2022)
{
	naños <- (añof-añoi)+1
      año <- rep(añoi:añof)
	maxy <- 0
      ta.base <- rep(0,naños)
      ta.a <- rep(0,naños)
      ta.b <- rep(0,naños)
      ta.c <- rep(0,naños)
	#ta.imr <- rep(0,naños)
	#ta.ld <- rep(0,naños)
	ta.ldapc <- rep(0,naños)
	ta.ldapc.p <- rep(0,naños)
      tm <- rep(0,naños)
      tm.p <- rep(0,naños)
      TAA <- as.numeric(tam.2022[tam.2022$CODIGO==icdo & tam.2022$SEXO==sexo,"A"])
      TAB <- as.numeric(tam.2022[tam.2022$CODIGO==icdo & tam.2022$SEXO==sexo,"B"])
      TAC <- as.numeric(tam.2022[tam.2022$CODIGO==icdo & tam.2022$SEXO==sexo,"C"])
      TALDAPC <- as.numeric(tam.2022[tam.2022$CODIGO==icdo & tam.2022$SEXO==sexo,"LDA"])
      #TAIMR <- tam.2022[tam.2022$CODIGO==icdo,"IMR"]
      #TALD <- tam.2022[tam.2022$CODIGO==icdo,"LD"]
      muertos <- prediccion(añof,icdo,sexo)
      muertos <- as.numeric(muertos[,4:21])
      if (sexo=="M") {
            pob <- read.table("poblacion-M-2022-nordpred.txt", header = T, sep = ",", row.names = 1)
      }
      if (sexo=="F") {
            pob <- read.table("poblacion-F-2022-nordpred.txt", header = T, sep = ",", row.names = 1)
    	}        
      pob <- pob[,1]
      wne <- matrix(c(0.05, 0.055, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07, 0.07, 0.07, 0.065, 0.06, 0.055, 0.05, 0.04, 0.025, 0.025),nrow = 18, ncol = 1)
      TM <- (muertos %*% (wne/pob)) * 100000
      for(i in añoi:añof)
      {
        	if(i<=uañoi)
            {
                	ta.base[i-(añoi-1)] <- ta.estimada(100, icdo, sexo, i,i, 1)
                  ta.a[i-(añoi-1)] <- NA
                  ta.b[i-(añoi-1)] <- NA
                  ta.c[i-(añoi-1)] <- NA
                  #ta.imr[i-(añoi-1)] <- NA
                  #ta.ld[i-(añoi-1)] <- NA
			ta.ldapc[i-(añoi-1)] <- proyeccion.ta(101, icdo, sexo, uañoi, i)
			ta.ldapc.p[i-(añoi-1)] <- NA
			tm[i-(añoi-1)] <- ta("M",100, icdo, sexo, i, i, 1,18,1)
                  tm.p[i-(añoi-1)] <- NA
                  if (max(ta.base[i-(añoi-1)],ta.ldapc[i-(añoi-1)]) > maxy)
                  {
				maxy <- max(ta.base[i-(añoi-1)],ta.ldapc[i-(añoi-1)])
                  }
    		}
            if(i>uañoi)
            {
                  ta.base[i-(añoi-1)] <- NA
                  ta.a[i-(añoi-1)] <- (ta.estimada(100, icdo, sexo, uañoi, uañoi, 1)) + ((TAA-ta.estimada(100, icdo, sexo, uañoi, uañoi, 1))  * ((i-uañoi)/(añof-uañoi)) )
                  ta.b[i-(añoi-1)] <- (ta.estimada(100, icdo, sexo, uañoi, uañoi, 1)) + ((TAB-ta.estimada(100, icdo, sexo, uañoi, uañoi, 1))  * ((i-uañoi)/(añof-uañoi)) )
                  ta.c[i-(añoi-1)] <- (ta.estimada(100, icdo, sexo, uañoi, uañoi, 1)) + ((TAC-ta.estimada(100, icdo, sexo, uañoi, uañoi, 1))  * ((i-uañoi)/(añof-uañoi)) )
                  #ta.imr[i-(añoi-1)] <- TAIMR
                  #ta.ld[i-(añoi-1)] <- TALD
			ta.ldapc[i-(añoi-1)] <- NA
                  ta.ldapc.p[i-(añoi-1)] <- proyeccion.ta(101, icdo, sexo, uañoi, i)
                  if(i<=uañom) 
          		{
                		tm[i-(añoi-1)] <- ta("M",100, icdo, sexo, i, i, 1,18, 1)
                 		tm.p[i-(añoi-1)] <- NA
          		}
                  if(i>uañom) 
           		{
               		tm[i-(añoi-1)] <- NA
                 		tm.p[i-(añoi-1)] <- (ta("M",100, icdo, sexo, 2019, 2019, 1,18,1)) + ((TM-ta("M",100, icdo, sexo, 2019, 2019, 1,18,1))  * ((i-2019)/(añof-2019)) )
                 	}
                  if (max(ta.a[i-(añoi-1)],ta.b[i-(añoi-1)],ta.c[i-(añoi-1)],ta.ldapc.p[i-(añoi-1)]) > maxy)
                  {
                       	maxy <- max(ta.a[i-(añoi-1)],ta.b[i-(añoi-1)],ta.c[i-(añoi-1)],ta.ldapc.p[i-(añoi-1)])
                  }
           }
	}
	plot(año,ta.base,type="l", main="Evolución de la nueva tasa ajustada europea", font=2, cex.lab=1.2, cex.main=1.5,las=2, xlab="AÑO", ylab="TAne", lwd=2, col=4, tck=1, xaxt="n", ylim = c(0,maxy))
	points(año,ta.a, lwd=2, col="blue", pch=1)
      points(año,ta.b, lwd=2, col="blue", pch=2)
      points(año,ta.c, lwd=2, col="blue", pch=5)
	#points(año,ta.imr, lwd=2, col="black", pch=3)
      #points(año,ta.ld, lwd=2, col="brown", pch=4)
      lines(año,ta.ldapc,type="l", lwd=2, col="brown")
      points(año,ta.ldapc.p, lwd=2, col="brown", pch=19)
      lines(año,tm,type="l", lwd=2, col=2)
      points(año,tm.p, lwd=2, col="red")
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=1, las=3, font=2,)
}




grafica.rim <- function(icdo, sexo, añoi=2001, añof=2022)
{
	naños <- (añof-añoi)+1
	año <- rep(añoi:añof)
	n <- rep(1:naños, 0)
	datos <- evolucion.rim(icdo,sexo,"C",añoi,añof,13)[,2]
	datos.a <- evolucion.rim(icdo,sexo,"A",añoi,añof,13)[,2]
	datos.b <- evolucion.rim(icdo,sexo,"B",añoi,añof,13)[,2]
	datos.c <- evolucion.rim(icdo,sexo,"C",añoi,añof,13)[,2]
	maxy <- max(datos,datos.a,datos.b,datos.c)
	for(i in añoi:añof)
	{
		if(i<=2015) {
			datos.a[i-(añoi-1)] <- NA
			datos.b[i-(añoi-1)] <- NA
			datos.c[i-(añoi-1)] <- NA
		}
		if(i>2015) {
			datos[i-(añoi-1)] <- NA
		}
	}
	plot(año,datos,type="l", main="Evolución de la RIM", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="AÑO", ylab="RIM", lwd=2, col=4, tck=1, xaxt="n", ylim = c(0,maxy))
	points(año,datos.a, lwd=2, col=2, pch=1)
        points(año,datos.b, lwd=2, col=2, pch=2)
        points(año,datos.c, lwd=2, col=2, pch=5)
	axis(side=1, año, tcl=-0.2, labels=FALSE, las=3)
	mtext(año, side=1, las=1, at=año, line=0.3, cex=1, las=3, font=2,)
}

grafica.edad.incidencia <- function(icdo,sexo,añoi=2011,añof=2015)
{
	casos <- rep(0,18)
	edad <- rep(1:18)
	edad2 <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
	pob <- poblacion[poblacion$ZONA==100 & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,4:21]
	aux <- rep(0,dim(pob)[1])
	pob <- aggregate(pob,list(aux=aux),sum)
	pob <- pob[2:19]
	rm(aux)
	for (año in añoi:añof) {
		casos <- casos + n.estimada.edad(100,icdo,sexo,año)
	}
	tee <- (casos/pob)*100000
	tee <- t(as.matrix(tee))
	maxy <- max(tee)
	plot(edad,tee,type="o", main="Gráfica Incidencia por edad", font=2, cex.lab=1.2, cex.main=1.5, las=2, xlab="EDAD", ylab="TEE", tck=1, lwd=2, col=4, xaxt="n", ylim = c(0,maxy))
	lines(edad,tee,type="l", lwd=2, col=4)
        axis(side=1, edad, tcl=-0.2, labels=FALSE, las=3)
	mtext(edad2, side=1, las=1, at=edad, line=0.3, cex=1, las=3, font=2,)
}


# FUNCIONES PREPARACION DATOS NORDPRED Y PREDICCION DE MORTALIDAD
###################################################################

cuartiles <- function(icdo,sexo)
{
	datos <- mortalidad[mortalidad$ZONA==100 & mortalidad$ICDO==icdo & mortalidad$SEXO==sexo,]	
	datos <- aggregate(datos[,5:22],list(ICDO=datos$ICDO, SEXO=datos$SEXO),sum)
	datos <- datos[,3:20]
      suma <- sum(datos)
	frecuencia <- (datos/suma)*100
        delta1 <- 100
        delta2 <- 100
        delta3 <- 100
        delta4 <- 100
	for (i in 1:18) {
        	suma.sup <- sum(frecuencia[,1:i])
        	if (abs(10-suma.sup) < delta1) {
			delta1 <- abs(10-suma.sup)
			corteedad <- i
                        p10 <- i*5
			}
		if (abs(33.3333-suma.sup) < delta2) {
			delta2 <- abs(33.3333-suma.sup)
			t2 <- i*5
			}
		if (abs(66.6666-suma.sup) < delta3) {
			delta3 <- abs(66.6666-suma.sup)
			t3 <- i*5
			}
		if (abs(90-suma.sup) < delta4) {
			delta4 <- abs(90-suma.sup)
			p90 <- i*5
			}
		}
	aux <- c(icdo, sexo, corteedad,p10,t2,t3,p90)
	aux
}	

prep.nordpred.tumor <- function(año,icdo,sexo,zona=100)
{
matriz <- matrix(data = rep(0,95), nrow = 19, ncol = 5)
if (año==2020) { matriz[1,1:5] <- c("row.names","98-02","03-07","08-12","13-17") }
if (año==2021) { matriz[1,1:5] <- c("row.names","99-03","04-08","09-13","14-18") }
if (año==2022) { matriz[1,1:5] <- c("row.names","00-04","05-09","10-14","15-19") }
matriz[2:19,1] <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
for (i in 1:18) 
	{
	matriz[i+1,2] <- n.total("M",zona,icdo,sexo,año-22,año-18,i,i)
	matriz[i+1,3] <- n.total("M",zona,icdo,sexo,año-17,año-13,i,i)
	matriz[i+1,4] <- n.total("M",zona,icdo,sexo,año-12,año-8,i,i)
	matriz[i+1,5] <- n.total("M",zona,icdo,sexo,año-7,año-3,i,i)
	}	
matriz
}

prep.nordpred <- function(año,icdo,sexo,zona=100)
{
nombre.tumor <- paste0("tumor-",icdo,"-",sexo,"-",año,".txt")
tumor <- prep.nordpred.tumor(año,icdo,sexo,zona)
write(t(tumor),file=nombre.tumor,ncolumns=5,sep=",")
}

prep.nordpred.lista <- function(año=2022)
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		sexo <- parametros.tumor$SEXO[i]
		if(parametros.tumor$TIPO[i] != "0")
		{
			prep.nordpred(año,icdo,sexo)

		}
	}
}

pob.total <- function(zona=100, sexo="M", añoi=2001, añof=2022, edin=1, edfn=18)
{
	a<-3+edin
	b<-3+edfn
	pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO>=añoi & poblacion$AÑO<=añof,a:b]
	sum(pob)
}

prep.nordpred.zona <- function(año, zona=100, sexo="M")
{
matriz <- matrix(data = rep(0,95), nrow = 19, ncol = 5)
if (año==2020) { matriz[1,1:5] <- c("row.names","98-02","03-07","08-12","13-17") }
if (año==2021) { matriz[1,1:5] <- c("row.names","99-03","04-08","09-13","14-18") }
if (año==2022) { matriz[1,1:5] <- c("row.names","00-04","05-09","10-14","15-19") }
matriz[2:19,1] <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
for (i in 1:18) {
	matriz[i+1,2] <- pob.total(zona,sexo,año-22,año-18,i,i)
	matriz[i+1,3] <- pob.total(zona,sexo,año-17,año-13,i,i)
	matriz[i+1,4] <- pob.total(zona,sexo,año-12,año-8,i,i)
	matriz[i+1,5] <- pob.total(zona,sexo,año-7,año-3,i,i)
	}
matriz
}

prep.nordpred.zona.año <- function(año, zona=100, sexo="M")
{
matriz <- matrix(data = rep(0,38), nrow = 19, ncol = 2)
if (año==2020) { matriz[1,1:2] <- c("row.names","18-22") }
if (año==2021) { matriz[1,1:2] <- c("row.names","19-23") }
if (año==2022) { matriz[1,1:2] <- c("row.names","20-24") }
matriz[2:19,1] <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
for (i in 1:18) {
	matriz[i+1,2] <- pob.total(zona,sexo,año,año,i,i)
	}
matriz
}

prep.nordpred.sexo <- function(año=2022, sexo="M")
{
nombre.archivo <- paste0("Poblacion-",sexo,"-",año,".txt")
archivo <- prep.nordpred.zona(año,100,sexo)
write(t(archivo),file=nombre.archivo,ncolumns=5,sep=",")
nombre.archivo <- paste0("Poblacion-",sexo,"-",año,"-nordpred.txt")
archivo <- prep.nordpred.zona.año(año,100,sexo)
write(t(archivo),file=nombre.archivo,ncolumns=2,sep=",")
}

lista.prediccion <- function(año=2022)
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		sexo <- parametros.tumor$SEXO[i]
		if(parametros.tumor$TIPO[i] != "0")
		{
			if(i==1)
			{
				salida <-  prediccion(año,icdo,sexo)
			}
			if(i!=1)
			{
				salida <- rbind(salida,prediccion(año,icdo,sexo))
			}	
		}
	}
	salida <- as.data.frame(salida)
	names(salida) <- c("ICDO","SEXO","AÑO","M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18")
	salida
}

prediccion <- function(año,icdo,sexo) 
{
      nombre.tumor <- paste0("tumor-",icdo,"-",sexo,"-",año,".txt")
	indata <- read.table(nombre.tumor, header = T, sep = ",", row.names = 1)
	nombre.archivo.nordpred <- paste0("Poblacion-",sexo,"-",año,"-nordpred.txt")
	nombre.archivo <- paste0("Poblacion-",sexo,"-",año,".txt")
	inpop1 <- read.table(nombre.archivo, header = T, sep = ",", row.names = 1)	# ----
	inpop2 <- read.table(nombre.archivo.nordpred, header = T, sep = ",", row.names = 1)
	inpop <- cbind(inpop1, inpop2)
	est <- nordpred.estimate(indata, inpop, 4, 12)
	res <- nordpred.prediction(est, 12, recent=T,cuttrend = c(0, 0.25, 0.5, 0.75, 0.75))
	prediccion.año <- t(res$predictions[,5])
	prediccion.año <- cbind(icdo,sexo,año,prediccion.año)
	prediccion.año
}

prediccion.mortalidad.año <- function(año,icdo,sexo)
{
	salida <- prediccion.muertos[prediccion.muertos$ICDO==icdo & prediccion.muertos$SEXO==sexo & prediccion.muertos$AÑO==año, 4:21]
	salida
}

# FUNCIONES PREPARACION DE DATOS Y SIMULACION
###############################################


# Funcion de salida de datos de incidencia, mortalidad, poblacion.

salida.datos.registro <- function(tipo, zona, icdo,sexo)
{
	añoi <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOI"]
	añof <- parametros.registro[parametros.registro$REGISTRO == zona,"AÑOF"]
	if (tipo=="I")  	{
		salida <- incidencia[incidencia$ZONA == zona & incidencia$SEXO == sexo & incidencia$ICDO == icdo & incidencia$AÑO >=añoi & incidencia$AÑO <=añof,]
		}
	if (tipo=="M")  	{
		salida <- mortalidad[mortalidad$ZONA == zona & mortalidad$SEXO == sexo & mortalidad$ICDO == icdo & mortalidad$AÑO >=añoi & mortalidad$AÑO <=añof,]
		}
	if (tipo=="P")  	{
		salida <- poblacion[poblacion$ZONA == zona & poblacion$SEXO == sexo & poblacion$AÑO >=añoi & poblacion$AÑO <=añof,]
	}
	salida
}

salida.datos <- function(tipo,icdo,sexo)
{
	n <- nrow(parametros.registro[parametros.registro$REGISTRO <100,])
	for(i in 1:n)
	{
		zona <- parametros.registro$REGISTRO[i]
		{
			if(i==1)
			{
				salida <- salida.datos.registro(tipo,zona,icdo,sexo)
			}
			if(i!=1)
			{
				salida <- rbind(salida,salida.datos.registro(tipo,zona,icdo,sexo))
			}	
		}
	}
	salida
}


# Preparacion de datos para analisis RIM

datos.rim <- function(icdo,sexo)
{
	corteedad <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"CORTEEDAD"]
	incidencia.rim <- salida.datos("I",icdo,sexo)
	mortalidad.rim <- salida.datos("M",icdo,sexo)
	datos.rim <- merge(incidencia.rim, mortalidad.rim,by=c("ZONA","AÑO","SEXO","ICDO"))
	datos.rim
	datos.rim$TOT <- rowSums(datos.rim[,5:23])
      datos.rim$p <- rep(1,nrow(datos.rim))
      datos.rim[datos.rim$TOT>0,]$p <- (datos.rim[datos.rim$TOT>0,]$TOT-datos.rim[datos.rim$TOT>0,]$SE)/datos.rim[datos.rim$TOT>0,]$TOT
	datos.rim$M01 <- datos.rim$M01*datos.rim$p
	datos.rim$M02 <- datos.rim$M02*datos.rim$p
	datos.rim$M03 <- datos.rim$M03*datos.rim$p
	datos.rim$M04 <- datos.rim$M04*datos.rim$p
	datos.rim$M05 <- datos.rim$M05*datos.rim$p
	datos.rim$M06 <- datos.rim$M06*datos.rim$p
	datos.rim$M07 <- datos.rim$M07*datos.rim$p
	datos.rim$M08 <- datos.rim$M08*datos.rim$p
	datos.rim$M09 <- datos.rim$M09*datos.rim$p
	datos.rim$M10 <- datos.rim$M10*datos.rim$p
	datos.rim$M11 <- datos.rim$M11*datos.rim$p
	datos.rim$M12 <- datos.rim$M12*datos.rim$p
	datos.rim$M13 <- datos.rim$M13*datos.rim$p
	datos.rim$M14 <- datos.rim$M14*datos.rim$p
	datos.rim$M15 <- datos.rim$M15*datos.rim$p
	datos.rim$M16 <- datos.rim$M16*datos.rim$p
	datos.rim$M17 <- datos.rim$M17*datos.rim$p
	datos.rim$M18 <- datos.rim$M18*datos.rim$p
	kf1 <- 5 + corteedad - 1
	ki1 <- 5 + corteedad
	kf2 <- 24 + corteedad - 1
	ki2 <- 24 + corteedad
	datos.rim$I00 <- rowSums(datos.rim[5:kf1])
	datos.rim$M00 <- rowSums(datos.rim[24:kf2])
	aux1 <- names(datos.rim[1:2])
	aux2 <- names(datos.rim[ki1:22])
	aux3 <- names(datos.rim[ki2:41])
	aux <-cbind(t(aux1),"I00",t(aux2),"M00",t(aux3))
	datos.rim <- datos.rim[,aux]
	datos.rim.t <- melt(datos.rim, id=c("ZONA","AÑO"))
	n <- dim(datos.rim.t)[1]
	kf <- n/2
	ki <- kf+1
	datos.rim.t1 <- datos.rim.t[1:kf,]
	datos.rim.t1$EDAD <- as.numeric(substr(as.character(datos.rim.t1$variable),2,3))
	datos.rim.t2 <- datos.rim.t[ki:n,]
	datos.rim.t2$EDAD <- as.numeric(substr(as.character(datos.rim.t2$variable),2,3))
	datos.rim <- merge(datos.rim.t1, datos.rim.t2,by=c("ZONA","AÑO","EDAD"))
	datos.rim$X <- datos.rim$value.x
	datos.rim$Y <- datos.rim$value.y
	aux <- c("ZONA","AÑO","EDAD","X","Y")
	datos.rim <- datos.rim[,aux]
	datos.rim
}

correcion.rim.edad <- function(icdo,sexo)
{
	corteedad <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"CORTEEDAD"]
	prov <- (EDAD*5) - 2.5
      prov[EDAD==0] <- (corteedad*5)-2.5
      prov[EDAD==18] <- 90
      prov
}

correcion.rim.zero <- function()
{
	prov <- Y
	prov[Y==0] <- exp(-3)
	prov
}

preparacion.datos.tumor<-function(icdo,sexo)
{
	datos.tumor <- datos.rim(icdo,sexo)
	datos.tumor <- datos.tumor[(datos.tumor$X != 0 | datos.tumor$Y != 0),]
	attach(datos.tumor)
	datos.tumor$Y <- correcion.rim.zero()
	datos.tumor$EDAD <- correcion.rim.edad(icdo,sexo)
	datos.tumor$AÑO <- as.numeric(datos.tumor$AÑO) - 2001
	datos.tumor$AÑO2 <- as.numeric(datos.tumor$AÑO) * as.numeric(datos.tumor$AÑO)
	detach()
	q1 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P10"]
	q2 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T2"]
	q3 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T3"]
	q4 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P90"]
	attach(datos.tumor)
	datos.tumor$EDAD1 <- pmax(0,EDAD-q1)
	datos.tumor$EDAD2 <- pmax(0,EDAD-q2)
	datos.tumor$EDAD3 <- pmax(0,EDAD-q3)
	datos.tumor$EDAD4 <- pmax(0,EDAD-q4)
	detach()
	rm(q1,q2,q3,q4)
	datos.tumor
}

prep.openbugs <- function(icdo,sexo)
{
	nombre.tumor <- paste0("tumor-",icdo,"-",sexo,".txt")
	datos.tumor <- preparacion.datos.tumor(icdo,sexo)
	write.csv2(datos.tumor,file=nombre.tumor,row.names=FALSE)
} 

simulacion <- function(datos, tipo = "P2", cadenas=3, iteraciones=50000, calentamiento=10000, intervalo=5)
{
	if (tipo == "SE") {
		aux <- c("ZONA","AÑO","AÑO2","X","Y")
		datos <- datos[,aux] 
		datos <- aggregate(datos,list(ZONA=datos$ZONA,AÑO=datos$AÑO,AÑO2=datos$AÑO2),sum)
		datos <- datos[,aux]
		rm(aux)
		print(datos)
	}
	J <- nrow(datos)
	X <- datos$X
	Y <- datos$Y
        if (tipo == "P2" | tipo == "P1" | tipo == "SE" | tipo == "SP") {
		year <- datos$AÑO
	}
        if (tipo == "P2" | tipo == "SE"| tipo == "SP") {
		year2 <- datos$AÑO2
	}
	if (tipo != "SE") {
		edad1 <- datos$EDAD1
		edad2 <- datos$EDAD2
		edad3 <- datos$EDAD3
		edad4 <- datos$EDAD4
	}
	ID <- datos$ZONA
	ID <- factor(ID)
	if (tipo == "P2") {
		data <- list(J=J, X=X, Y=Y, ID=ID, year=year, year2=year2, edad1=edad1, edad2=edad2, edad3=edad3, edad4=edad4)
		parameters <- c("alpha", "a", "YEAR", "YEAR2", "EDAD1", "EDAD2", "EDAD3", "EDAD4", "tau","sigma")
		inits <- function() {list (alpha=rnorm(6), a=rnorm(1), YEAR=rnorm(1), YEAR2=rnorm(1,0,0.1), EDAD1=rnorm(1), EDAD2=rnorm(1), EDAD3=rnorm(1), EDAD4=rnorm(1), tau=1)}
                #datos.sim <- bugs(data, inits, parameters, "model-02.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo, bugs.directory = "C:/Users/5148/Documents/WinBUGS14", program = "WinBUGS",debug=TRUE)
		datos.sim <- jags(data, inits, parameters, "model-02.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo)
		rm(datos,J,X,Y,year,year2,edad1,edad2,edad3,edad4,ID,data,parameters,inits)
	}
	if (tipo == "P1") {
		data <- list(J=J, X=X, Y=Y, ID=ID, year=year, edad1=edad1, edad2=edad2, edad3=edad3, edad4=edad4)
		parameters <- c("alpha", "a", "YEAR", "EDAD1", "EDAD2", "EDAD3", "EDAD4", "tau","sigma")
		inits <- function() {list (alpha=rnorm(6), a=rnorm(1), YEAR=rnorm(1), EDAD1=rnorm(1), EDAD2=rnorm(1), EDAD3=rnorm(1), EDAD4=rnorm(1), tau=1)}
                #datos.sim <- bugs(data, inits, parameters, "model-01.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo, bugs.directory = "C:/Users/5148/Documents/WinBUGS14", program = "WinBUGS")
		datos.sim <- jags(data, inits, parameters, "model-01.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo)
		rm(datos,J,X,Y,year,edad1,edad2,edad3,edad4,ID,data,parameters,inits)
	}
	if (tipo == "P0") {
		data <- list(J=J, X=X, Y=Y, ID=ID, edad1=edad1, edad2=edad2, edad3=edad3, edad4=edad4)
		parameters <- c("alpha", "a", "EDAD1", "EDAD2", "EDAD3", "EDAD4", "tau","sigma")
		inits <- function() {list (alpha=rnorm(6), a=rnorm(1), EDAD1=rnorm(1), EDAD2=rnorm(1), EDAD3=rnorm(1), EDAD4=rnorm(1), tau=1)}
                #datos.sim <- bugs(data, inits, parameters, "model-00.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo, bugs.directory = "C:/Users/5148/Documents/WinBUGS14", program = "WinBUGS")
		datos.sim <- jags(data, inits, parameters, "model-00.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo)
		rm(datos,J,X,Y,edad1,edad2,edad3,edad4,ID,data,parameters,inits)
	}
	if (tipo == "SE") {
		data <- list(J=J, X=X, Y=Y, ID=ID, year=year, year2=year2)
		parameters <- c("alpha", "a", "YEAR", "YEAR2", "tau","sigma")
		inits <- function() {list (alpha=rnorm(6), a=rnorm(1), YEAR=rnorm(1), YEAR2=rnorm(1,0,0.1), tau=1)}
                #datos.sim <- bugs(data, inits, parameters, "model-se.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo, bugs.directory = "C:/Users/5148/Documents/WinBUGS14", program = "WinBUGS")
		datos.sim <- jags(data, inits, parameters, "model-se.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo)
		rm(datos,J,X,Y,year,year2,ID,data,parameters,inits)
	}
	if (tipo == "SP") {
		data <- list(J=J, X=X, Y=Y, year=year, year2=year2, edad1=edad1, edad2=edad2, edad3=edad3, edad4=edad4)
		parameters <- c("a", "YEAR", "YEAR2", "EDAD1", "EDAD2", "EDAD3", "EDAD4")
		inits <- function() {list (a=rnorm(6), YEAR=rnorm(1), YEAR2=rnorm(1,0,0.1), EDAD1=rnorm(1), EDAD2=rnorm(1), EDAD3=rnorm(1), EDAD4=rnorm(1))}
                #datos.sim <- bugs(data, inits, parameters, "model-sp.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo, bugs.directory = "C:/Users/5148/Documents/WinBUGS14", program = "WinBUGS")
		datos.sim <- jags(data, inits, parameters, "model-sp.bug", n.chains=cadenas, n.iter=iteraciones, n.burnin=calentamiento, n.thin=intervalo)
		rm(datos,J,X,Y,year,year2,edad1,edad2,edad3,edad4,data,parameters,inits)
	}
	datos.sim
}

simulacion.icdo <- function(icdo, sexo, tipo = "P2", cadenas=3, iteraciones=50000, calentamiento=10000, intervalo=5)
{
	prep.openbugs(icdo,sexo)
	nombre.tumor <- paste0("tumor-",icdo,"-",sexo,".txt")
	datos <- read.csv2(nombre.tumor, header=TRUE)
	datos.sim <- simulacion(datos,tipo,cadenas,iteraciones,calentamiento,intervalo)
	result.sim <- datos.sim$BUGSoutput$sims.matrix
        nombre.tumor2 <- paste0("simulacion-",icdo,"-",sexo,".txt")
        write.csv2(result.sim,nombre.tumor2,row.names=FALSE)
	print(datos.sim,dig=6)
}


# EXTRACCION DE DATOS PARA CREAR TABLA RIM
###########################################


# Creación archivo tabla.rim

tabla.rim.icdo <- function(icdo, sexo, tipo)
{
	nombre.tumor <- paste0("simulacion-",icdo,"-",sexo,".txt")
	result.sim <- read.csv2(nombre.tumor, header=TRUE)
	if(tipo == "P2" | tipo == "SE") {
		alpha.1. <- median(result.sim$alpha.1.)
        	alpha.2. <- median(result.sim$alpha.2.)
        	alpha.3. <- median(result.sim$alpha.3.)
        	alpha.4. <- median(result.sim$alpha.4.)
        	alpha.5. <- median(result.sim$alpha.5.)
        	alpha.6. <- median(result.sim$alpha.6.)
		}
	if(tipo == "SP") {
		alpha.1. <- 0
        	alpha.2. <- 0
        	alpha.3. <- 0
        	alpha.4. <- 0
        	alpha.5. <- 0
        	alpha.6. <- 0
		}
        a <- median(result.sim$a)
        YEAR <- median(result.sim$YEAR)
        YEAR2 <- median(result.sim$YEAR2)
        if(tipo=="P2" | tipo == "SP")
		{
			EDAD1 <-median(result.sim$EDAD1)
			EDAD2 <-median(result.sim$EDAD2)
			EDAD3 <-median(result.sim$EDAD3)
			EDAD4 <-median(result.sim$EDAD4)
		}
		if(tipo=="SE")
		{
			EDAD1 <-0
			EDAD2 <-0
			EDAD3 <-0
			EDAD4 <-0
		}	
	aux <- cbind(icdo, sexo, alpha.1.,alpha.2.,alpha.3.,alpha.4.,alpha.5.,alpha.6.,a,YEAR,YEAR2,EDAD1,EDAD2,EDAD3,EDAD4)
	aux
}

extraer.tabla.rim <- function()
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		if(parametros.tumor$TIPO[i] != "0")
		{
			tipo <- parametros.tumor$TIPO[i]
			icdo <- parametros.tumor$CODIGO[i]
			sexo <- parametros.tumor$SEXO[i]
			if(i==1)
			{
				salida <- tabla.rim.icdo(icdo, sexo,tipo)
			}
			if(i!=1)
			{
				salida <- rbind(salida, tabla.rim.icdo(icdo,sexo,tipo))
			}	
		}
	}
	salida <- as.data.frame(salida)
	names(salida) <- c("ICDO","SEXO","alpha.1.","alpha.2.","alpha.3.","alpha.4.","alpha.5.","alpha.6.","a","YEAR","YEAR2","EDAD1","EDAD2","EDAD3","EDAD4")
	salida
}	


# PROYECCIONES

coe.u.año <- function(año,icdo,sexo,escenario)
{
    modelo <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO==sexo,"TIPO"] 
    q1 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO==sexo,"P10"]
    q2 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO==sexo,"T2"]
    q3 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO==sexo,"T3"]
    q4 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO==sexo,"P90"] 
    nombre.tumor <- paste0("simulacion-",icdo,"-",sexo,".txt")
    result.sim <- read.csv2(nombre.tumor, header=TRUE)
    coe.a <- result.sim$a
    if(modelo=="P2" | modelo =="SE") {
       coe.sigma <- result.sim$sigma
       }
    if(modelo=="SP") {
       coe.sigma <- rep(0,24000)
       }
    AÑO <- result.sim$YEAR
    AÑO2 <- result.sim$YEAR2
    if(modelo=="P2" | modelo =="SP")
        {
        EDAD1 <- result.sim$EDAD1
        EDAD2 <- result.sim$EDAD2
        EDAD3 <- result.sim$EDAD3
        EDAD4 <- result.sim$EDAD4
        }
    if(modelo=="SE")
        {
        EDAD1 <- rep(0,24000)
        EDAD2 <- rep(0,24000)
        EDAD3 <- rep(0,24000)
        EDAD4 <- rep(0,24000)
        }
    if (escenario=="A")
    {
        coe.año <- (2015-2001)*AÑO + ((2015-2001)*(2015-2001))*AÑO2
    }

    if (escenario=="B")
    {
	coe.año.2001 <- (2001-2001)*AÑO + ((2001-2001)*(2001-2001))*AÑO2
        coe.año.2015 <- (2015-2001)*AÑO + ((2015-2001)*(2015-2001))*AÑO2
	incremento <- coe.año.2015 - coe.año.2001
        coe.año <- coe.año.2001 + (incremento*((año-2001)/(2015-2001)))
    }
    if (escenario=="C")
    {
        coe.año <- (año-2001)*AÑO + ((año-2001)*(año-2001))*AÑO2
    }
    coe.edad1 <- EDAD1*pmax(0,2.5-q1)
    coe.edad2 <- EDAD2*pmax(0,2.5-q2)
    coe.edad3 <- EDAD3*pmax(0,2.5-q3)
    coe.edad4 <- EDAD4*pmax(0,2.5-q4)
    coe.edad.01 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.01 <- coe.a + coe.año + coe.edad.01
    coe.edad1 <- EDAD1*pmax(0,7.5-q1)
    coe.edad2 <- EDAD2*pmax(0,7.5-q2)
    coe.edad3 <- EDAD3*pmax(0,7.5-q3)
    coe.edad4 <- EDAD4*pmax(0,7.5-q4)
    coe.edad.02 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.02 <- coe.a + coe.año + coe.edad.02    
    coe.edad1 <- EDAD1*pmax(0,12.5-q1)
    coe.edad2 <- EDAD2*pmax(0,12.5-q2)
    coe.edad3 <- EDAD3*pmax(0,12.5-q3)
    coe.edad4 <- EDAD4*pmax(0,12.5-q4)
    coe.edad.03 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.03 <- coe.a + coe.año + coe.edad.03    
    coe.edad1 <- EDAD1*pmax(0,17.5-q1)
    coe.edad2 <- EDAD2*pmax(0,17.5-q2)
    coe.edad3 <- EDAD3*pmax(0,17.5-q3)
    coe.edad4 <- EDAD4*pmax(0,17.5-q4)
    coe.edad.04 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.04 <- coe.a + coe.año + coe.edad.04    
    coe.edad1 <- EDAD1*pmax(0,22.5-q1)
    coe.edad2 <- EDAD2*pmax(0,22.5-q2)
    coe.edad3 <- EDAD3*pmax(0,22.5-q3)
    coe.edad4 <- EDAD4*pmax(0,22.5-q4)
    coe.edad.05 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.05 <- coe.a + coe.año + coe.edad.05    
    coe.edad1 <- EDAD1*pmax(0,27.5-q1)
    coe.edad2 <- EDAD2*pmax(0,27.5-q2)
    coe.edad3 <- EDAD3*pmax(0,27.5-q3)
    coe.edad4 <- EDAD4*pmax(0,27.5-q4)
    coe.edad.06 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.06 <- coe.a + coe.año + coe.edad.06    
    coe.edad1 <- EDAD1*pmax(0,32.5-q1)
    coe.edad2 <- EDAD2*pmax(0,32.5-q2)
    coe.edad3 <- EDAD3*pmax(0,32.5-q3)
    coe.edad4 <- EDAD4*pmax(0,32.5-q4)
    coe.edad.07 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.07 <- coe.a + coe.año + coe.edad.07    
    coe.edad1 <- EDAD1*pmax(0,37.5-q1)
    coe.edad2 <- EDAD2*pmax(0,37.5-q2)
    coe.edad3 <- EDAD3*pmax(0,37.5-q3)
    coe.edad4 <- EDAD4*pmax(0,37.5-q4)
    coe.edad.08 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.08 <- coe.a + coe.año + coe.edad.08    
    coe.edad1 <- EDAD1*pmax(0,42.5-q1)
    coe.edad2 <- EDAD2*pmax(0,42.5-q2)
    coe.edad3 <- EDAD3*pmax(0,42.5-q3)
    coe.edad4 <- EDAD4*pmax(0,42.5-q4)
    coe.edad.09 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.09 <- coe.a + coe.año + coe.edad.09    
    coe.edad1 <- EDAD1*pmax(0,47.5-q1)
    coe.edad2 <- EDAD2*pmax(0,47.5-q2)
    coe.edad3 <- EDAD3*pmax(0,47.5-q3)
    coe.edad4 <- EDAD4*pmax(0,47.5-q4)
    coe.edad.10 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.10 <- coe.a + coe.año + coe.edad.10    
    coe.edad1 <- EDAD1*pmax(0,52.5-q1)
    coe.edad2 <- EDAD2*pmax(0,52.5-q2)
    coe.edad3 <- EDAD3*pmax(0,52.5-q3)
    coe.edad4 <- EDAD4*pmax(0,52.5-q4)
    coe.edad.11 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.11 <- coe.a + coe.año + coe.edad.11    
    coe.edad1 <- EDAD1*pmax(0,57.5-q1)
    coe.edad2 <- EDAD2*pmax(0,57.5-q2)
    coe.edad3 <- EDAD3*pmax(0,57.5-q3)
    coe.edad4 <- EDAD4*pmax(0,57.5-q4)
    coe.edad.12 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.12 <- coe.a + coe.año + coe.edad.12    
    coe.edad1 <- EDAD1*pmax(0,62.5-q1)
    coe.edad2 <- EDAD2*pmax(0,62.5-q2)
    coe.edad3 <- EDAD3*pmax(0,62.5-q3)
    coe.edad4 <- EDAD4*pmax(0,62.5-q4)
    coe.edad.13 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.13 <- coe.a + coe.año + coe.edad.13    
    coe.edad1 <- EDAD1*pmax(0,67.5-q1)
    coe.edad2 <- EDAD2*pmax(0,67.5-q2)
    coe.edad3 <- EDAD3*pmax(0,67.5-q3)
    coe.edad4 <- EDAD4*pmax(0,67.5-q4)
    coe.edad.14 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.14 <- coe.a + coe.año + coe.edad.14    
    coe.edad1 <- EDAD1*pmax(0,72.5-q1)
    coe.edad2 <- EDAD2*pmax(0,72.5-q2)
    coe.edad3 <- EDAD3*pmax(0,72.5-q3)
    coe.edad4 <- EDAD4*pmax(0,72.5-q4)
    coe.edad.15 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.15 <- coe.a + coe.año + coe.edad.15    
    coe.edad1 <- EDAD1*pmax(0,77.5-q1)
    coe.edad2 <- EDAD2*pmax(0,77.5-q2)
    coe.edad3 <- EDAD3*pmax(0,77.5-q3)
    coe.edad4 <- EDAD4*pmax(0,77.5-q4)
    coe.edad.16 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.16 <- coe.a + coe.año + coe.edad.16    
    coe.edad1 <- EDAD1*pmax(0,82.5-q1)
    coe.edad2 <- EDAD2*pmax(0,82.5-q2)
    coe.edad3 <- EDAD3*pmax(0,82.5-q3)
    coe.edad4 <- EDAD4*pmax(0,82.5-q4)
    coe.edad.17 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.17 <- coe.a + coe.año + coe.edad.17    
    coe.edad1 <- EDAD1*pmax(0,90-q1)
    coe.edad2 <- EDAD2*pmax(0,90-q2)
    coe.edad3 <- EDAD3*pmax(0,90-q3)
    coe.edad4 <- EDAD4*pmax(0,90-q4)
    coe.edad.18 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.18 <- coe.a + coe.año + coe.edad.18
    coe.u <- cbind(coe.01,coe.02,coe.03,coe.04,coe.05,coe.06,coe.07,coe.08,coe.09,coe.10,coe.11,coe.12,coe.13,coe.14,coe.15,coe.16,coe.17,coe.18)
    coe.u     
    }

coe.sigma <- function(icdo,sexo) 
{
    nombre.tumor <- paste0("simulacion-",icdo,"-",sexo,".txt")
    result.sim <- read.csv2(nombre.tumor, header=TRUE)
    modelo <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"TIPO"] 
    if(modelo=="P2" | modelo =="SE") {
       coe.sigma <- result.sim$sigma
       }
    if(modelo=="SP") {
       coe.sigma <- rep(0,24000)
       }
    coe.sigma
}

coe.u.model <- function(año,icdo,sexo,escenario)
{
    modelo <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo ,"TIPO"] 
    q1 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P10"]
    q2 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T2"]
    q3 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"T3"]
    q4 <- parametros.tumor[parametros.tumor$CODIGO == icdo & parametros.tumor$SEXO == sexo,"P90"] 
    RIM <- tabla.rim[tabla.rim$ICDO == icdo & tabla.rim$SEXO == sexo,]
    coe.a <- RIM$a
    coe.sigma <- 0
    AÑO <- RIM$YEAR
    AÑO2 <- RIM$YEAR2
    if(modelo=="P2" | modelo =="SP")
        {
        EDAD1 <- RIM$EDAD1
        EDAD2 <- RIM$EDAD2
        EDAD3 <- RIM$EDAD3
        EDAD4 <- RIM$EDAD4
        }
    if(modelo=="SE")
        {
        EDAD1 <- 0
        EDAD2 <- 0
        EDAD3 <- 0
        EDAD4 <- 0
        }
    if (escenario=="A")
    {
        coe.año <- (2015-2001)*AÑO + ((2015-2001)*(2015-2001))*AÑO2
    }

    if (escenario=="B")
    {
	coe.año.2001 <- (2001-2001)*AÑO + ((2001-2001)*(2001-2001))*AÑO2
        coe.año.2015 <- (2015-2001)*AÑO + ((2015-2001)*(2015-2001))*AÑO2
	incremento <- coe.año.2015 - coe.año.2001
        coe.año <- coe.año.2001 + (incremento*((año-2001)/(2015-2001)))
    }
    if (escenario=="C")
    {
        coe.año <- (año-2001)*AÑO + ((año-2001)*(año-2001))*AÑO2
    }
    coe.edad1 <- EDAD1*pmax(0,2.5-q1)
    coe.edad2 <- EDAD2*pmax(0,2.5-q2)
    coe.edad3 <- EDAD3*pmax(0,2.5-q3)
    coe.edad4 <- EDAD4*pmax(0,2.5-q4)
    coe.edad.01 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.01 <- coe.a + coe.año + coe.edad.01
    coe.edad1 <- EDAD1*pmax(0,7.5-q1)
    coe.edad2 <- EDAD2*pmax(0,7.5-q2)
    coe.edad3 <- EDAD3*pmax(0,7.5-q3)
    coe.edad4 <- EDAD4*pmax(0,7.5-q4)
    coe.edad.02 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.02 <- coe.a + coe.año + coe.edad.02    
    coe.edad1 <- EDAD1*pmax(0,12.5-q1)
    coe.edad2 <- EDAD2*pmax(0,12.5-q2)
    coe.edad3 <- EDAD3*pmax(0,12.5-q3)
    coe.edad4 <- EDAD4*pmax(0,12.5-q4)
    coe.edad.03 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.03 <- coe.a + coe.año + coe.edad.03    
    coe.edad1 <- EDAD1*pmax(0,17.5-q1)
    coe.edad2 <- EDAD2*pmax(0,17.5-q2)
    coe.edad3 <- EDAD3*pmax(0,17.5-q3)
    coe.edad4 <- EDAD4*pmax(0,17.5-q4)
    coe.edad.04 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.04 <- coe.a + coe.año + coe.edad.04    
    coe.edad1 <- EDAD1*pmax(0,22.5-q1)
    coe.edad2 <- EDAD2*pmax(0,22.5-q2)
    coe.edad3 <- EDAD3*pmax(0,22.5-q3)
    coe.edad4 <- EDAD4*pmax(0,22.5-q4)
    coe.edad.05 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.05 <- coe.a + coe.año + coe.edad.05    
    coe.edad1 <- EDAD1*pmax(0,27.5-q1)
    coe.edad2 <- EDAD2*pmax(0,27.5-q2)
    coe.edad3 <- EDAD3*pmax(0,27.5-q3)
    coe.edad4 <- EDAD4*pmax(0,27.5-q4)
    coe.edad.06 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.06 <- coe.a + coe.año + coe.edad.06    
    coe.edad1 <- EDAD1*pmax(0,32.5-q1)
    coe.edad2 <- EDAD2*pmax(0,32.5-q2)
    coe.edad3 <- EDAD3*pmax(0,32.5-q3)
    coe.edad4 <- EDAD4*pmax(0,32.5-q4)
    coe.edad.07 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.07 <- coe.a + coe.año + coe.edad.07    
    coe.edad1 <- EDAD1*pmax(0,37.5-q1)
    coe.edad2 <- EDAD2*pmax(0,37.5-q2)
    coe.edad3 <- EDAD3*pmax(0,37.5-q3)
    coe.edad4 <- EDAD4*pmax(0,37.5-q4)
    coe.edad.08 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.08 <- coe.a + coe.año + coe.edad.08    
    coe.edad1 <- EDAD1*pmax(0,42.5-q1)
    coe.edad2 <- EDAD2*pmax(0,42.5-q2)
    coe.edad3 <- EDAD3*pmax(0,42.5-q3)
    coe.edad4 <- EDAD4*pmax(0,42.5-q4)
    coe.edad.09 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.09 <- coe.a + coe.año + coe.edad.09    
    coe.edad1 <- EDAD1*pmax(0,47.5-q1)
    coe.edad2 <- EDAD2*pmax(0,47.5-q2)
    coe.edad3 <- EDAD3*pmax(0,47.5-q3)
    coe.edad4 <- EDAD4*pmax(0,47.5-q4)
    coe.edad.10 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.10 <- coe.a + coe.año + coe.edad.10    
    coe.edad1 <- EDAD1*pmax(0,52.5-q1)
    coe.edad2 <- EDAD2*pmax(0,52.5-q2)
    coe.edad3 <- EDAD3*pmax(0,52.5-q3)
    coe.edad4 <- EDAD4*pmax(0,52.5-q4)
    coe.edad.11 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.11 <- coe.a + coe.año + coe.edad.11    
    coe.edad1 <- EDAD1*pmax(0,57.5-q1)
    coe.edad2 <- EDAD2*pmax(0,57.5-q2)
    coe.edad3 <- EDAD3*pmax(0,57.5-q3)
    coe.edad4 <- EDAD4*pmax(0,57.5-q4)
    coe.edad.12 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.12 <- coe.a + coe.año + coe.edad.12    
    coe.edad1 <- EDAD1*pmax(0,62.5-q1)
    coe.edad2 <- EDAD2*pmax(0,62.5-q2)
    coe.edad3 <- EDAD3*pmax(0,62.5-q3)
    coe.edad4 <- EDAD4*pmax(0,62.5-q4)
    coe.edad.13 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.13 <- coe.a + coe.año + coe.edad.13    
    coe.edad1 <- EDAD1*pmax(0,67.5-q1)
    coe.edad2 <- EDAD2*pmax(0,67.5-q2)
    coe.edad3 <- EDAD3*pmax(0,67.5-q3)
    coe.edad4 <- EDAD4*pmax(0,67.5-q4)
    coe.edad.14 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.14 <- coe.a + coe.año + coe.edad.14    
    coe.edad1 <- EDAD1*pmax(0,72.5-q1)
    coe.edad2 <- EDAD2*pmax(0,72.5-q2)
    coe.edad3 <- EDAD3*pmax(0,72.5-q3)
    coe.edad4 <- EDAD4*pmax(0,72.5-q4)
    coe.edad.15 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.15 <- coe.a + coe.año + coe.edad.15    
    coe.edad1 <- EDAD1*pmax(0,77.5-q1)
    coe.edad2 <- EDAD2*pmax(0,77.5-q2)
    coe.edad3 <- EDAD3*pmax(0,77.5-q3)
    coe.edad4 <- EDAD4*pmax(0,77.5-q4)
    coe.edad.16 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.16 <- coe.a + coe.año + coe.edad.16    
    coe.edad1 <- EDAD1*pmax(0,82.5-q1)
    coe.edad2 <- EDAD2*pmax(0,82.5-q2)
    coe.edad3 <- EDAD3*pmax(0,82.5-q3)
    coe.edad4 <- EDAD4*pmax(0,82.5-q4)
    coe.edad.17 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.17 <- coe.a + coe.año + coe.edad.17    
    coe.edad1 <- EDAD1*pmax(0,90-q1)
    coe.edad2 <- EDAD2*pmax(0,90-q2)
    coe.edad3 <- EDAD3*pmax(0,90-q3)
    coe.edad4 <- EDAD4*pmax(0,90-q4)
    coe.edad.18 <- coe.edad1+coe.edad2+coe.edad3+coe.edad4
    coe.18 <- coe.a + coe.año + coe.edad.18
    coe.u <- cbind(coe.01,coe.02,coe.03,coe.04,coe.05,coe.06,coe.07,coe.08,coe.09,coe.10,coe.11,coe.12,coe.13,coe.14,coe.15,coe.16,coe.17,coe.18)
    coe.u     
    }

projeccion <- function(año,icdo,sexo,escenario,n.sim=1000)
{
    u <- coe.u.año(año,icdo,sexo,escenario)
    sigma <- coe.sigma(icdo,sexo)
    muertos <- prediccion.muertos[prediccion.muertos$ICDO==icdo & prediccion.muertos$SEXO==sexo & prediccion.muertos$AÑO==año,]
    muertos <- muertos[,4:21]
    if (sexo=="M") { nombre.file <- paste0("poblacion-M","-",año,"-nordpred.txt") }
    if (sexo=="F") { nombre.file <- paste0("poblacion-F","-",año,"-nordpred.txt") }
    pob <- read.table(nombre.file, header = T, sep = ",", row.names = 1)
    pob <- pob[,1]
    pobtotal <- sum(pob)
    UNO <- rep(1,18)
    wm <- matrix(c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005),nrow = 18, ncol = 1)
    we <- matrix(c(0.08, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.01),nrow = 18, ncol = 1)
    wne <- matrix(c(0.05, 0.055, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07, 0.07, 0.07, 0.065, 0.06, 0.055, 0.05, 0.04, 0.025, 0.025),nrow = 18, ncol = 1)
    n <- rep(0,n.sim)
    ni <- rep(0,n.sim)
    ns <- rep(0,n.sim)
    tb <- rep(0,n.sim)
    tbi <- rep(0,n.sim)
    tbs <- rep(0,n.sim)
    tam <- rep(0,n.sim)
    tami <- rep(0,n.sim)
    tams <- rep(0,n.sim)
    tae <- rep(0,n.sim)
    taei <- rep(0,n.sim)
    taes <- rep(0,n.sim)
    tane <- rep(0,n.sim)
    tanei <- rep(0,n.sim)
    tanes <- rep(0,n.sim)
    set.seed(112358)
    for (i in 1:n.sim) {
        alpha.01 <- rnorm(24000,1,sigma)
        alpha.02 <- rnorm(24000,1,sigma)
        alpha.03 <- rnorm(24000,1,sigma)
        alpha.04 <- rnorm(24000,1,sigma)
        alpha.05 <- rnorm(24000,1,sigma)
        alpha.06 <- rnorm(24000,1,sigma)
        alpha.07 <- rnorm(24000,1,sigma)
        alpha.08 <- rnorm(24000,1,sigma)
        alpha.09 <- rnorm(24000,1,sigma)
        alpha.10 <- rnorm(24000,1,sigma)
        alpha.11 <- rnorm(24000,1,sigma)
        alpha.12 <- rnorm(24000,1,sigma)
        alpha.13 <- rnorm(24000,1,sigma)
        alpha.14 <- rnorm(24000,1,sigma)
        alpha.15 <- rnorm(24000,1,sigma)
        alpha.16 <- rnorm(24000,1,sigma)
        alpha.17 <- rnorm(24000,1,sigma)
        alpha.18 <- rnorm(24000,1,sigma)
        alpha <- cbind(alpha.01,alpha.02,alpha.03,alpha.04,alpha.05,alpha.06,alpha.07,alpha.08,alpha.09,alpha.10,alpha.11,alpha.12,alpha.13,alpha.14,alpha.15,alpha.16,alpha.17,alpha.18)
        alpha[alpha < 0] <- 0
        RIM <- exp(u) * alpha
                ESP.01<-RIM[,1]*as.numeric(muertos$M01)
                ESP.02<-RIM[,2]*as.numeric(muertos$M02)
                ESP.03<-RIM[,3]*as.numeric(muertos$M03)
                ESP.04<-RIM[,4]*as.numeric(muertos$M04)
                ESP.05<-RIM[,5]*as.numeric(muertos$M05)
                ESP.06<-RIM[,6]*as.numeric(muertos$M06)
                ESP.07<-RIM[,7]*as.numeric(muertos$M07)
                ESP.08<-RIM[,8]*as.numeric(muertos$M08)
                ESP.09<-RIM[,9]*as.numeric(muertos$M09)
                ESP.10<-RIM[,10]*as.numeric(muertos$M10)
                ESP.11<-RIM[,11]*as.numeric(muertos$M11)
                ESP.12<-RIM[,12]*as.numeric(muertos$M12)
                ESP.13<-RIM[,13]*as.numeric(muertos$M13)
                ESP.14<-RIM[,14]*as.numeric(muertos$M14)
                ESP.15<-RIM[,15]*as.numeric(muertos$M15)
                ESP.16<-RIM[,16]*as.numeric(muertos$M16)
                ESP.17<-RIM[,17]*as.numeric(muertos$M17)
                ESP.18<-RIM[,18]*as.numeric(muertos$M18)
        N.01 <- rpois(24000,ESP.01)
        N.02 <- rpois(24000,ESP.02)
        N.03 <- rpois(24000,ESP.03)
        N.04 <- rpois(24000,ESP.04)
        N.05 <- rpois(24000,ESP.05)
        N.06 <- rpois(24000,ESP.06)
        N.07 <- rpois(24000,ESP.07)
        N.08 <- rpois(24000,ESP.08)
        N.09 <- rpois(24000,ESP.09)
        N.10 <- rpois(24000,ESP.10)
        N.11 <- rpois(24000,ESP.11)
        N.12 <- rpois(24000,ESP.12)
        N.13 <- rpois(24000,ESP.13)
        N.14 <- rpois(24000,ESP.14)
        N.15 <- rpois(24000,ESP.15)
        N.16 <- rpois(24000,ESP.16)
        N.17 <- rpois(24000,ESP.17)
        N.18 <- rpois(24000,ESP.18)
        CASOS <- cbind(N.01,N.02,N.03,N.04,N.05,N.06,N.07,N.08,N.09,N.10,N.11,N.12,N.13,N.14,N.15,N.16,N.17,N.18)
        N <- CASOS %*% UNO
        TB <- (N/pobtotal) * 100000
        TAM <- (CASOS %*% (wm/pob)) * 100000
        TAE <- (CASOS %*% (we/pob)) * 100000
	TANE <- (CASOS %*% (wne/pob)) * 100000
        solucion <- quantile(N,p=c(0.025,0.5,0.975))
        n[i] <- solucion[2]
        ni[i] <- solucion[1]
        ns[i] <- solucion[3]
        solucion <- quantile(TB,p=c(0.025,0.5,0.975))
        tb[i] <- solucion[2]
        tbi[i] <- solucion[1]
        tbs[i] <- solucion[3]
        solucion <- quantile(TAM,p=c(0.025,0.5,0.975))
        tam[i] <- solucion[2]
        tami[i] <- solucion[1]
        tams[i] <- solucion[3]
        solucion <- quantile(TAE,p=c(0.025,0.5,0.975))
        tae[i] <- solucion[2]
        taei[i] <- solucion[1]
        taes[i] <- solucion[3]
        solucion <- quantile(TANE,p=c(0.025,0.5,0.975))
        tane[i] <- solucion[2]
        tanei[i] <- solucion[1]
        tanes[i] <- solucion[3]
        }
    sol.n <- median(n)
    sol.ni <- median(ni)
    sol.ns <- median(ns)
    sol.tb <- median(tb)
    sol.tbi <- median(tbi)
    sol.tbs <- median(tbs)
    sol.tam <- median(tam)
    sol.tami <- median(tami)
    sol.tams <- median(tams)
    sol.tae <- median(tae)
    sol.taei <- median(taei)
    sol.taes <- median(taes)
    sol.tane <- median(tane)
    sol.tanei <- median(tanei)
    sol.tanes <- median(tanes)
    salida <- c(icdo,sexo,año,escenario,sol.n, sol.ni, sol.ns,sol.tb, sol.tbi, sol.tbs,sol.tam, sol.tami, sol.tams,sol.tae, sol.taei, sol.taes,sol.tane, sol.tanei, sol.tanes)
    salida
}


lista.proyeccion.imr <- function(año, escenario,n.sims=100)
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		if(parametros.tumor$TIPO[i] != "0")
		{
			icdo <- parametros.tumor$CODIGO[i]
			sexo <- parametros.tumor$SEXO[i]
			if(i==1)
			{
				salida <- projeccion(año,icdo,sexo,escenario,n.sims)
			}
			if(i!=1)
			{
				salida <- rbind(salida, projeccion(año,icdo,sexo,escenario,n.sims))
			}	
		}
	}
      salida <- as.data.frame(salida)
      names(salida) <- c("CODIGO","SEXO","AÑO","METODO","N","N_INF","N_SUP","TB","TB_INF","TB_SUP","TAM","TAM_INF","TAM_SUP","TAE","TAE_INF","TAE_SUP","TANE","TANE_INF","TANE_SUP")
      salida
}

projeccion.tee <- function(año, icdo, sexo, escenario)
{
    u <- coe.u.model(año,icdo,sexo,escenario)
    muertos <- prediccion.muertos[prediccion.muertos$ICDO==icdo & prediccion.muertos$SEXO==sexo & prediccion.muertos$AÑO==año,]
    poblacion <- poblacion[poblacion$SEXO==sexo & poblacion$AÑO==año & poblacion$ZONA==100,]
    muertos <- muertos[,4:21]
    pob <- poblacion[,4:21]
    RIM <- exp(u) 
    tee.01<-(RIM[,1]*muertos[,1])/pob[,1]
    tee.02<-(RIM[,2]*muertos[,2])/pob[,2]
    tee.03<-(RIM[,3]*muertos[,3])/pob[,3]
    tee.04<-(RIM[,4]*muertos[,4])/pob[,4]
    tee.05<-(RIM[,5]*muertos[,5])/pob[,5]
    tee.06<-(RIM[,6]*muertos[,5])/pob[,6]
    tee.07<-(RIM[,7]*muertos[,7])/pob[,7]
    tee.08<-(RIM[,8]*muertos[,8])/pob[,8]
    tee.09<-(RIM[,9]*muertos[,9])/pob[,9]
    tee.10<-(RIM[,10]*muertos[,10])/pob[,10]
    tee.11<-(RIM[,11]*muertos[,11])/pob[,11]
    tee.12<-(RIM[,12]*muertos[,12])/pob[,12]
    tee.13<-(RIM[,13]*muertos[,13])/pob[,13]
    tee.14<-(RIM[,14]*muertos[,14])/pob[,14]
    tee.15<-(RIM[,15]*muertos[,15])/pob[,15]
    tee.16<-(RIM[,16]*muertos[,16])/pob[,16]
    tee.17<-(RIM[,17]*muertos[,17])/pob[,17]
    tee.18<-(RIM[,18]*muertos[,18])/pob[,18]
    tee <- cbind(año,icdo,escenario,tee.01,tee.02,tee.03,tee.04,tee.05,tee.06,tee.07,tee.08,tee.09,tee.10,tee.11,tee.12,tee.13,tee.14,tee.15,tee.16,tee.17,tee.18)
    tee
}

lista.projeccion.tee <- function(año)
{
    n <- nrow(parametros.tumor)
    salida <- c("AÑO","ICDO","ESCENARIO","T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","T11","T12","T13","T14","T15","T16","T17","T18")
    for(i in 1:n)
    {
        if(parametros.tumor$ESCENARIO[i] == "A" | parametros.tumor$ESCENARIO[i] == "B" | parametros.tumor$ESCENARIO[i] == "C")
        {
            icdo <- parametros.tumor$CODIGO[i]
            escenario <- parametros.tumor$ESCENARIO[i]
		sexo <- parametros.tumor$SEXO[i]
            salida <- rbind(salida,projeccion.tee(año,icdo,sexo,escenario))
        }
     }
     salida
}


# FUNCIONES PARA EL CALCULO POR ESTIMACION DIRECTA
###################################################

# Funciones auxiliares

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

# Funciones de tendencias

datos.apc <- function(zona=101, icdo=100, sexo="M", añoi=2006, añof=2015)
{
	casos.inc <- incidencia.0115[incidencia.0115$ZONA == zona & incidencia.0115$ICDO == icdo & incidencia.0115$SEXO== sexo & incidencia.0115$AÑO >=  añoi & incidencia.0115$AÑO <= añof,]
	pobs <- poblacion.0115[poblacion.0115$ZONA == zona & poblacion.0115$SEXO == sexo & poblacion.0115$AÑO >=  añoi & poblacion.0115$AÑO <= añof,]
	datos <- merge(casos.inc,pobs,by=c("ZONA","SEXO","AÑO"))
	aux <- c("AÑO","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10","I11","I12","I13","I14","I15","I16","I17","I18")
	casos.inc <- datos[,aux]
	aux <- c("AÑO","P01","P02","P03","P04","P05","P06","P07","P08","P09","P10","P11","P12","P13","P14","P15","P16","P17","P18")
	pobs <- datos[,aux]
	aux <- c("AÑO","G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12","G13","G14","G15","G16","G17","G18")
	names(casos.inc) <- aux
	names(pobs) <- aux
	pobs <- melt(pobs,id=c("AÑO"))
	casos.inc <- melt(casos.inc,id=c("AÑO"))
	datos <- merge(casos.inc,pobs,by=c("AÑO","variable"))
	aux <- c("AÑO","EDAD","X","POB")
	names(datos) <- aux
	datos
}

trend <- function(zona=101, icdo=100, sexo="M", añoi=2006, añof=2015)
{
	datos <- datos.apc(zona, icdo, sexo, añoi, añof)
	global <- glm(X ~ AÑO + EDAD + offset(log(POB)), family = poisson, data = datos)
	tit <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo, "VALOR"]	
	tit <- paste(c(as.character(tit)), collapse = "", sep = " ")
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
	if(is.null(n)) 
	{
		n <- 1
		dim(e) <- c(1, 4)
		dimnames(e) <- list(dnam[pos], names(e))
	}
	for(i in 1:n) 
	{
		cat(format(tit, width=50), fmt.n(nn, 6, 0),"   ",fmt.n(e[i, 1],10, 7),"   ",fmt.n(e[i, 2], 10, 7),"\n")
	}
}


lista.trend <- function(zona=101, añoi=2006, añof=2015)
{
    n <- nrow(parametros.tumor)
    for(i in 1:n)
    {
        if(parametros.tumor$ESCENARIO[i] != "Especial")
        {
            icdo <- parametros.tumor$CODIGO[i]
		sexo <- parametros.tumor$SEXO[i]
            print(trend(zona, icdo, sexo, añoi, añof))
        }
     }
}

# Funciones proyeccion

tee.proyeccion <- function(zona=101, icdo=100, sexo="M", añoi=2011, añof=2015)
{
	pob <- poblacion.0115[poblacion.0115$ZONA==zona & poblacion.0115$SEXO==sexo & poblacion.0115$AÑO>=añoi & poblacion.0115$AÑO<=añof,4:21]
	aux <- rep(0,dim(pob)[1])
	pob <- aggregate(pob,list(aux=aux),sum)
	pob <- pob[2:19]
	rm(aux) 
	n <- nrow(incidencia.0115[incidencia.0115$ZONA==zona & incidencia.0115$ICDO==icdo & incidencia.0115$SEXO==sexo & incidencia.0115$AÑO>=añoi & incidencia.0115$AÑO<=añof,5:23]) 
	if (n>0) {casos <- incidencia.0115[incidencia.0115$ZONA==zona & incidencia.0115$ICDO==icdo & incidencia.0115$SEXO==sexo & incidencia.0115$AÑO>=añoi & incidencia.0115$AÑO<=añof,5:23]}
                if (n==0) {casos <- t(rep(0,18))} 	
	aux <- rep(0,dim(casos)[1])
	mis <- 0
	casos <- aggregate(casos,list(aux=aux),sum)
	if (n>0) {mis <- casos[,20]}
	casos <- casos[,2:19]
	rm(aux) 
	tasa <- (casos/pob)*100000
	tot <- sum(casos)
	if (tot>0) {
		tasa <- tasa * (tot/(tot - mis))
	}
	tasa 
}


proyeccion.tasas <- function(zona=101, icdo=100, sexo="M", añof=2015, añop=2022, metodo="T", base = 5, r = 3, m = 20)
{
	añoi <- (añof-base+1)
	añob <- (añoi + añof)/2
	if(metodo=="T" | metodo=="A") { tee <- tee.proyeccion(zona,icdo,sexo,añoi,añof)}
	if(metodo=="IMR") {tee <- tee.estimada(zona,icdo,sexo,añoi,añof)}
	beta <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo,"BETA"]
	sdbeta <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo,"SDBETA"]
	beta2 <- min(max(log(1-(r/100)),beta),log(1+(r/100)))
	sdbeta2 <- sdbeta * (beta2/beta)
	apc <- (100 * (exp(beta2)-1))
	if (metodo=="A" | metodo=="IMR" ) { apc <- 0 }
	b <- 1+(apc/100)
	if(m != 0) { a <- (apc/100) * (1/m) }
	if(m == 0) { a <- 0 }
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

proyeccion.n <- function(zona=101, icdo=100, sexo="M", añof=2015, añop=2022, edin=1, edfn=18, metodo="T", base = 5, r = 3, m = 20)
{
	if (icdo != 100)
	{
		edades <- rep(0,18)
		edades[edin:edfn] <- 1
		tee <- proyeccion.tasas(zona, icdo, sexo, añof, añop, metodo, base, r, m) * edades
		pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]
		if(zona==101) {pob <- poblacion[poblacion$ZONA==100 & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]}
		n <- as.matrix(tee) %*% as.matrix(t(pob))
		n <- n/100000
	}
	if (icdo == 100) 
	{
		n <- 0
		k <- nrow(parametros.tumor)
		for (i in 1:k) {
			tumor <- parametros.tumor$CODIGO[i]
			if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i] == sexo) 
			{
			 	n <- n + proyeccion.n(zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,r,m) 
		 	}
		}
	}
	n
}

proyeccion.ta <- function(zona=101, icdo=100, sexo="M", añof=2015, añop=2022, edin=1, edfn=18, estandard=1, metodo="T", base = 5, r = 3, m = 20)
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
		if(añop > añof) {
			tee <- proyeccion.tasas(zona, icdo, sexo, añof, añop, metodo, base, r, m) * edades
			}
		if(añop <= añof) {
			tee <- tee.proyeccion(zona, icdo, sexo, añop, añop) * edades
			}
		ta <- as.matrix(tee) %*% w
	}
	if (icdo == 100) 
	{
		ta <- 0
		n <- nrow(parametros.tumor)
		for (i in 1:n) {
			tumor <- parametros.tumor$CODIGO[i]
			if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i] == sexo) 
			{
			 	ta <- ta + proyeccion.ta(zona,tumor,sexo,añof,añop,edin,edfn,estandard,metodo,base,r,m) 
		 	}
		}
	}
	ta
}

proyeccion.tb <- function(zona=101, icdo=100, sexo="M", añof=2015, añop=2022, edin=1, edfn=18, metodo="T", base = 5, r = 3, m = 20)
{
	if (icdo != 100)
	{
		edades <- rep(0,18)
		edades[edin:edfn] <- 1
		n <- proyeccion.n(zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,r,m)
		pob <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]
		if(zona==101) {pob <- poblacion[poblacion$ZONA==100 & poblacion$SEXO==sexo & poblacion$AÑO==añop, 4:21]}
		pob <- pob * edades
		pob <- sum(pob)
		tb <- (n/pob)*100000
	}
	if (icdo == 100) 
	{
		tb <- 0
		k <- nrow(parametros.tumor)
		for (i in 1:k) {
			tumor <- parametros.tumor$CODIGO[i]
			if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i] == sexo) 
			{
			 	tb <- tb + proyeccion.tb(zona,tumor,sexo,añof,añop,edin,edfn,metodo,base,r,m)
		 	}
		}
	}
	tb
}

indicadores.proyeccion <- function(zona=101, icdo=100, sexo="M", añof=2015, añop=2022, edin=1, edfn=18, metodo="T", base = 5, r = 3, m = 20, modo="P")
{
	valor.icdo <- parametros.tumor[parametros.tumor$CODIGO==icdo & parametros.tumor$SEXO==sexo, "VALOR"]
	valor.n <- proyeccion.n(zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,r,m)
	valor.tb <- proyeccion.tb(zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,r,m)
	valor.ta <- proyeccion.ta(zona,icdo,sexo,añof,añop,edin,edfn,2,metodo,base,r,m)
	valor.tae <- proyeccion.ta(zona,icdo,sexo,añof,añop,edin,edfn,3,metodo,base,r,m)
	valor.tane <- proyeccion.ta(zona,icdo,sexo,añof,añop,edin,edfn,1,metodo,base,r,m)
	if (modo=="P") 
	{
		cat(format(valor.icdo,width=50),fmt.n(valor.n,5,0),fmt.n(valor.tb,5,1),fmt.n(valor.ta,5,1),fmt.n(valor.tae,5,1),fmt.n(valor.tane,5,1),"\n")
	}
	if (modo=="DF") 
	{
		aux <- c(icdo,sexo,añop,"LDA",valor.n,valor.tb,valor.ta,valor.tae,valor.tane)
	}
}

lista.proyeccion.directa <- function(zona=101, sexo="M", añof=2015, añop=2022, edin=1, edfn=18, metodo="T", base = 5, r = 3, m = 20, modo="P")
{
	n <- nrow(parametros.tumor)
      if(modo=="P")
	{
		cat("","\n")
		tit.provincia <- as.character(parametros.registro[parametros.registro$REGISTRO==zona,"DESCRIPCION"])
		if (zona==101) {tit.provincia <- "TOTAL"}
		cat(tit.provincia," (",fmt.n(añop, 4, 0),")","\n")
		cat("------------------------------","\n")
		cat("","\n")
		if(sexo=="M") {
			cat("HOMBRES:","\n")
			}
		if(sexo=="F") {
			cat("MUJERES:","\n")
			}
		cat("","\n")
		cat("TIPO TUMORAL                                           N    TB   TAm   TAe  TAne ","\n")
		cat("-------------------------------------------------- ----- ----- ----- ----- -----","\n")
		salida <- c("")
	}
	if (modo=="DF")
	{
		salida <- c(0,"A",0,"A",0,0,0,0,0)
	}
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(parametros.tumor$TIPO[i] != "0" & parametros.tumor$SEXO[i]==sexo)
		{
			salida <- rbind(salida,indicadores.proyeccion(zona,icdo,sexo,añof,añop,edin,edfn,metodo,base,r,m,modo))
		}
	}
	if(modo=="P")
	{
		salida <- salida[-1,]
		cat("","\n")
	}
	if(modo=="DF")
	{
		salida <- salida[-1,]
		salida <- as.data.frame(salida)
		names(salida) <- c("CODIGO","SEXO","AÑO","METODO","N","TB","TAM","TAE","TANE")
		salida
	}
}

# Funciones TEE para el 2020 (revisar a 2022)

proyeccion.N.edad.imr <- function(año,icdo,sexo,escenario)
{
	if (escenario=="A") {
		RIM <- rim.estimada(100,icdo,sexo,2012)
		}
	if (escenario=="B") {
		rim.1998 <- rim.estimada(100,icdo,sexo,1998)
		rim.2012 <- rim.estimada(100,icdo,sexo,2012)
		rim.2022 <- rim.2012 + ((rim.2012-rim.1998)/(2012-1998)) * (año-2012)
		RIM <- rim.2022
		}
	if (escenario=="C") {
		RIM <- rim.estimada(100,icdo,sexo,año)
		}
    muertos <- prediccion.muertos[prediccion.muertos$ICDO==icdo & prediccion.muertos$SEXO==sexo & prediccion.muertos$AÑO==año,]
    muertos <- muertos[,4:21]
    nombre.file <- paste0("poblacion-",sexo,"-",año,"-nordpred.txt") 
    pob <- read.table(nombre.file, header = T, sep = ",", row.names = 1)
    pob <- pob[,1]
    UNO <- rep(1,18)
    N.01<-RIM[1]*muertos[,1]
    N.02<-RIM[2]*muertos[,2]
    N.03<-RIM[3]*muertos[,3]
    N.04<-RIM[4]*muertos[,4]
    N.05<-RIM[5]*muertos[,5]
    N.06<-RIM[6]*muertos[,6]
    N.07<-RIM[7]*muertos[,7]
    N.08<-RIM[8]*muertos[,8]
    N.09<-RIM[9]*muertos[,9]
    N.10<-RIM[10]*muertos[,10]
    N.11<-RIM[11]*muertos[,11]
    N.12<-RIM[12]*muertos[,12]
    N.13<-RIM[13]*muertos[,13]
    N.14<-RIM[14]*muertos[,14]
    N.15<-RIM[15]*muertos[,15]
    N.16<-RIM[16]*muertos[,16]
    N.17<-RIM[17]*muertos[,17]
    N.18<-RIM[18]*muertos[,18]
    CASOS <- cbind(N.01,N.02,N.03,N.04,N.05,N.06,N.07,N.08,N.09,N.10,N.11,N.12,N.13,N.14,N.15,N.16,N.17,N.18)
    TEE <- (CASOS/pob)*100000
    N <- CASOS %*% UNO
    TEE <- cbind(TEE,N)
    TEE
}
 
lista.proyeccion.N.edad <- function()
{
	n <- nrow(parametros.tumor)
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		sexo <- parametros.tumor$SEXO[i]
		escenario <- parametros.tumor$ESCENARIO[i]
		if(parametros.tumor$TIPO[i]!=0)
		{
			if(escenario=="A" | escenario == "B" | escenario == "C") {print(proyeccion.N.edad.imr(2022,icdo,sexo,escenario))}
			if(escenario=="IMR2012") {print(proyeccion.tasas(100,icdo,sexo,2012,2022,"IMR",base=1))}
			if(escenario=="LDA") {print(proyeccion.tasas(100,icdo,sexo,2012,2022,"T"))}
		}
	}
}

