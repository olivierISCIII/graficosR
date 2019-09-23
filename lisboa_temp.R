require(data.table)
require(ggplot2)
require(RColorBrewer)

temp.lisboa=fread("https://dominicroye.github.io/files/temp_lisboa.csv")
temp.lisboa[,ta:=ifelse(metANN<100,metANN,NA)]
col_strip <- brewer.pal(11, "RdBu")

ggplot(temp.lisboa,aes(x=YEAR,y=1,fill=ta))+
		geom_tile() +
		scale_fill_gradientn(colors = rev(col_strip)) +
        theme_void() + ggtitle(" LISBOA 1880-2018")
		