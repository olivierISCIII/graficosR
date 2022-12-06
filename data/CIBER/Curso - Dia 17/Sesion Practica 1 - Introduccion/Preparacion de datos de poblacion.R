# CODIGO PARA MODIFICAR UN ARCHIVO CON EDADES ANUALES EN 18 GRUPOS DE EDAD
###############################################################################################

# Lectura archivo casos

pobs <- read.csv2("pobs.csv",encoding="UTF-8")

names(pobs)[1] <- "ZONA"
names(pobs)[2] <- "AÑO"
names(pobs)[3] <- "SEXO"

n <- dim(pobs)[1]
m <- dim(pobs)[2]

pobs$P01 <- rep(0,n)
pobs$P02 <- rep(0,n)
pobs$P03 <- rep(0,n)
pobs$P04 <- rep(0,n)
pobs$P05 <- rep(0,n)
pobs$P06 <- rep(0,n)
pobs$P07 <- rep(0,n)
pobs$P08 <- rep(0,n)
pobs$P09 <- rep(0,n)
pobs$P10 <- rep(0,n)
pobs$P11 <- rep(0,n)
pobs$P12 <- rep(0,n)
pobs$P13 <- rep(0,n)
pobs$P14 <- rep(0,n)
pobs$P15 <- rep(0,n)
pobs$P16 <- rep(0,n)
pobs$P17 <- rep(0,n)
pobs$P18 <- rep(0,n)

for (i in 1:n)
{
	pobs$P01[i] <- sum(pobs[i,4:8])
	pobs$P02[i] <- sum(pobs[i,9:13])
	pobs$P03[i] <- sum(pobs[i,14:18])
	pobs$P04[i] <- sum(pobs[i,19:23])
	pobs$P05[i] <- sum(pobs[i,24:28])
	pobs$P06[i] <- sum(pobs[i,29:33])
	pobs$P07[i] <- sum(pobs[i,34:38])
	pobs$P08[i] <- sum(pobs[i,39:43])
	pobs$P09[i] <- sum(pobs[i,44:48])
	pobs$P10[i] <- sum(pobs[i,49:53])
	pobs$P11[i] <- sum(pobs[i,54:58])
	pobs$P12[i] <- sum(pobs[i,59:63])
	pobs$P13[i] <- sum(pobs[i,64:68])
	pobs$P14[i] <- sum(pobs[i,69:73])
	pobs$P15[i] <- sum(pobs[i,74:78])
	pobs$P16[i] <- sum(pobs[i,79:84])
	pobs$P17[i] <- sum(pobs[i,84:88])
	pobs$P18[i] <- sum(pobs[i,89:m])
}

aux <- c("ZONA","AÑO","SEXO","P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18")
pobs <- pobs[,aux]


