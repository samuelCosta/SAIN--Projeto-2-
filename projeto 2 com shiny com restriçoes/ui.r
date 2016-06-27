library(shiny)


# Define UI for miles per gallon application
shinyUI(fluidPage(

  # Application title
  titlePanel("Trabalho de SAIN"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarLayout( 
    sidebarPanel(
	    selectInput("periodo", "Qual o periodo que pertende optimizar?",c("------"="0","14/11/1011 a 20/11/1011"= "1","21/11/2011 a 27/11/2011"= "2","28/11/2011 a 04/12/2011"= "3"),selected = "0",width = NULL, size = NULL),
		selectInput("opt", "Qual o tipo de optimizacao?",c("------"="0","Simulated Annealing "= "1","climbing"= "2"," Montecarlo"= "3"),selected = "0",width = NULL, size = NULL)
		   
),
		mainPanel(
		
		plotOutput("periodo"),
		verbatimTextOutput("opt")
		
		
		
	#	textOutput("pe")
		
		

		)

	

 )))