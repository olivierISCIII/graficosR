# Lo siguiente subsana el problema con fortify

install.packages("rgeos")
require(rgeos)
require(ggmap)

## Luego se puede utilizar fortify y agrupar de acuerdo a las provincias

require(raster)  
shape <- getData("GADM", country= "Spain", level = 2) #mapa administrativo a nivel provincial
peninsula <- subset(shape,!NAME_1=="Islas Canarias") #mapa sin las islas canarias
peninsula.df=fortify(peninsula,region="CC_2") #convierte el shape en data.frames
paro=fread("data/paro.csv",encoding="UTF-8")
paro[,id:=sub(" ","0",format(Prov.id,width=2))]
Paro <- subset(paro,Año==2011 & Trimestre=="I")

peninsula.paro=merge(peninsula.df,Paro,by="id") #juntamos las dos bases

ggplot() + 
  geom_polygon(data = peninsula.paro, aes(long, lat, group = group,fill=Tasa.paro), colour = "grey80", size = .1) + 
  facet_grid(~ Sexo) + scale_fill_gradient(low="aliceblue",high="steelblue4")+coord_quickmap()