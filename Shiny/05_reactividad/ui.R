# Definción del UI
shinyUI(fluidPage(
  
# Titulo
titlePanel("Evolución de la esperanza de vida"),
  
sidebarPanel(width = 3,
		selectInput("continent", "Continente:", choices = gapminder$continent, selected=1)
	),
  
mainPanel(width = 9,
		plotOutput("evolucion"),
		br(),
		plotOutput("bigotes")			
	)
))