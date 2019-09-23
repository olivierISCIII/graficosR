# Definición del UI
shinyUI(fluidPage(
  
# Titulo
titlePanel("Una regresión"),


# Diseño de la interfaz
sidebarPanel(width=3,	
	selectInput("regresor",h4("Elegir variable de regresión:"),choices=names(mtcars)[2:5],selected="hp") 	   	  
	),
	
mainPanel(width=9, 	
	tabsetPanel(
		tabPanel("Regresión",
			plotOutput("regresion")
			),				
		tabPanel("Dotplot",
			plotOutput("dotplot")
			),
		tabPanel("Datos",
			tableOutput("datos")
			)	
		)
	)	
) )


