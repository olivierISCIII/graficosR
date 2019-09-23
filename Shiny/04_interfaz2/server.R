# Definición de la parte server
shinyServer(function(input, output) {

  output$dotplot <- renderPlot({
		qplot(mpg,reorder(modelo,mpg),data=mtcars,xlab="consumo (en mpg)",ylab="modelos")
	})
    
  output$regresion <- renderPlot({
    ggplot(mtcars,aes_string(input$regresor,"mpg"))+
		geom_text(aes(label=modelo),angle=10,check_overlap=TRUE)+
		geom_smooth(method='lm') 
	})  
  
  output$datos <-  renderTable(mtcars)
})
