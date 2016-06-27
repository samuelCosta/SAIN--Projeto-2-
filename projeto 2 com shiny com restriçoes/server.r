library(shiny)
#previsao do uk day
source("pred_uk_day.R")
#Tres metodos de optimizacao
source("Simulated_Annealing.R")
source("Funcmontecarlo.R")
source("climbing.R")

source("hill.r")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {


	output$periodo <- renderPlot({
			if(input$periodo!=0){
		if(input$periodo==1){ n=14
		pred_ukDay(n)}
		if(input$periodo==2){ n=7
		pred_ukDay(n)}
		if(input$periodo==3){ n=0
		pred_ukDay(n)}
		
		}
	
	})
	#output$pe <- renderText(input$periodo)
	
	output$opt <- renderPrint({
	w= input$opt
	
	if(input$opt != 0){	
	if(input$opt==1){ simulated_Annealing(w)}
	if(input$opt==2){ climbing(w)}
	if(input$opt==3){ Funcmontecarlo(w)} 
	
	}
	}
	
 )
	

})