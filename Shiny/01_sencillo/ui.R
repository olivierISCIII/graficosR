# Definción del UI
shinyUI(fluidPage(
  
	# Titulo
	titlePanel("Una regresión"),
  
	sidebarLayout(
	# Barra lateral
		sidebarPanel(
		  sliderInput("grosor","Tamaño de los puntos:",min = 0,max = 20,value = 10,step=2)
		),
    
	# Muestra el grafico en el panel principal
		mainPanel(
			plotOutput("grafico")
		)
  )
))
