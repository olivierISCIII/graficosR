tee.2025 <- read.csv2("tee.2025.csv",encoding="UTF-8")

edad <- c(4:15)
age <- (edad*5)-2.5
a <- rep(0,nrow(tee.2025))
k <- rep(0,nrow(tee.2025))
ca <- 0.005
ck <- 1
for (i in 1:nrow(tee.2025))
	{
	datos.tee <- tee.2025[i,6:17]
	tee<- as.numeric((datos.tee/100000))
	for(j in 1:12)
		{
		if(tee[j]<=0.000001){tee[j] <- 0.000001}
		}
	datos <- data.frame(age,tee)
	result <- nls(tee ~ ((a*age)^k),datos,start=list(a=ca,k=ck), trace=TRUE, algorithm="port",lower=c(0.000001,0.1), upper=c(1000000,10))
	a[i] <- summary(result)$coe[1,1]
	k[i] <- summary(result)$coe[2,1]
	}
	
rm(i,j,edad,age,ca,ck,datos.tee,tee,datos,result)

tee.2025 <- cbind(tee.2025[,1:2],a,k)

write.csv2(tee.2025,"incidencia.2025.csv", row.names=FALSE, fileEncoding="UTF-8")
