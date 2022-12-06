# PREPARACION DE DATOS JOINPOINT
################################

joinpoint <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi, añof, edin=1, edfn=18, estandard=1)
{
	salida <- c(0, 0, 0, 0, 0)
	for(i in añoi:añof)
	{
		salida <- rbind(salida,c(icdo, sexo, i, ta(tipo,zona,icdo,sexo,i,i,edin,edfn,estandard), tasd(tipo,zona,icdo,sexo,i,i,edin,edfn,estandard)))
	}
	salida <- salida[-1,]
	salida <- as.data.frame(salida)
	names(salida) <- c("ICDO", "SEXO", "YEAR", "TA", "SD_TA")
	salida$YEAR <- as.numeric(salida$YEAR)
	salida$TA <- as.numeric(salida$TA)
	salida$SD_TA <- as.numeric(salida$SD_TA)
	write.csv2(salida,"datos_JP.csv",row.names=FALSE)	
}

teesd <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi, añof)
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
	tasa <- (casos/(pob^2))*(100000*100000)
	tot <- n.total(tipo, zona, icdo, sexo, añoi, añof, 1, 18)	
	if (tot>0) {
		tasa <- tasa * (tot/(tot - mis))
	}
	tasa 
}

tasd <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi, añof, edin=1, edfn=18, estandard=1)
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
			tee <- teesd(tipo, zona, icdo, "M", añoi, añof)
			ta.m <- sqrt(as.matrix(tee) %*% (w*w))
			if (sexo=="M")
			{
				ta <- ta.m
			}
		}
		if((sexo=="F" | sexo=="A") & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1)
		{
			tee <- teesd(tipo, zona, icdo, "F", añoi, añof)
			ta.f <- sqrt(as.matrix(tee) %*% (w*w))
			if (sexo=="F")
			{
				ta <- ta.f
			}
		}
		if(sexo=="A")
		{
			if(parametros.tumor[parametros.tumor$CODIGO==icdo,"M"] >= 1 & parametros.tumor[parametros.tumor$CODIGO==icdo,"F"] >= 1) 
			{
	      		ta <- sqrt((ta.m^2) + (ta.f^2))
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



# FUNCIONES Y PREPARACION DATOS NORDPRED
########################################

# Ver EIR.RData


# FUNCIONES DESCOMPOSICION FACTORIAL
####################################

trend.bashir.esteve <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi, añof, base=5)
{
	valoricdo <- parametros.tumor[parametros.tumor$CODIGO==icdo,"VALOR"]
	
	pobi <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añoi,4:21]
	pobf <- poblacion[poblacion$ZONA==zona & poblacion$SEXO==sexo & poblacion$AÑO==añof,4:21]
	
	teebi <- tee(tipo, zona, icdo, sexo, añoi, añoi+(base-4))
	teebf <- tee(tipo, zona, icdo, sexo, añof-(base-4), añof)

	teei <- teebi * (ta(tipo, zona, icdo, sexo, añoi, añoi)) / ta(tipo, zona, icdo, sexo, añoi, añoi+(base-4))
	teef <- teebf * (ta(tipo, zona, icdo, sexo, añof, añof)) / ta(tipo, zona, icdo, sexo, añof-(base-4), añof)

	casosi <- sum(teei * pobi)
	casosf <- sum(teef * pobf)

	piramidei <- (pobi/sum(pobi))*100000
	piramidef <- (pobf/sum(pobf))*100000

	s1 <- sum(teei * piramidei)
	s2 <- sum(teef * piramidef)
	s3 <- sum(teei * piramidef)

	total <- ((casosf - casosi) / casosi)
	estructura <- ((s3-s1) / s1)
	riesgo <- ((s2-s3) / s1)
	poblacion <- total - estructura - riesgo 
	APC <- (((1+riesgo)^(1/(añof-añoi))) - 1) * 100

	cat(format(valoricdo,width=50),fmt.n(total*100,10,1),fmt.n(poblacion*100,10,1),fmt.n(estructura*100,10,1),fmt.n(riesgo*100,10,1),fmt.n(APC,10,1), "\n")
}

lista.trend.bashir.esteve <- function(tipo="I", zona=100, icdo=100, sexo="M", añoi, añof, base=5)
{
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
	cat("---------------------------------------------------------------------------------------------------------", "\n")
	cat("TUMOR                                                   TOTAL  POBLACIÓN ESTRUCTURA     RIESGO        APC", "\n")
	cat("-------------------------------------------------- ---------- ---------- ---------- ---------- ----------", "\n")
	for(i in 1:n)
	{
		icdo <- parametros.tumor$CODIGO[i]
		if(sexo=="M" & parametros.tumor$M[i] != "0")
		{
			trend.bashir.esteve(tipo,zona,icdo,sexo,añoi,añof,base=5)
	
		}
		if(sexo=="F" & parametros.tumor$F[i] != "0")
		{
			trend.bashir.esteve(tipo,zona,icdo,sexo,añoi,añof,base=5)
	
		}
		if(sexo=="A" & parametros.tumor$A[i] != "0")
		{
			trend.bashir.esteve(tipo,zona,icdo,sexo,añoi,añof,base=5)
	
		}
	}
	cat("","\n")
}

