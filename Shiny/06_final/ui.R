# Definción del UI
shinyUI(fluidPage(
  

titlePanel("Evolución del Paro"), # Titulo

sidebarPanel(width = 3,
		selectInput("sexo", "Sexo:", choices = c("Hombres","Mujeres"), selected="Mujeres"),
		selectInput("trimestre", "Trimestre:",choices = c("Todos","I","II","III","IV"),selected="Todos"),
		sliderInput("alpha","Visibilidad curvas provinciales", value = 10, min = 0, max =20,step=2,post="%")
		),
  
mainPanel(width = 9,
		tabsetPanel(id="Menu",
			tabPanel("Tendencia",
				br(),
				column(10,plotOutput("tendencia")),
				column(2,downloadButton("guardarTendencia", "Guardar (.pdf)"))
				),
			tabPanel("Datos",
				br(),
				fluidRow(column(2,offset=10,downloadButton("guardarTabla", "Guardar (.csv)"))),
				br(),
				fluidRow(column(12,dataTableOutput("datos")))
				)
		)			
	)
))

