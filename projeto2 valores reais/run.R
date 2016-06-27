#previsao do uk day
source("pred_uk_day.R")

#Tres metodos de optimizacao
source("Simulated_Annealing.R")
source("Funcmontecarlo.R")
source("climbing.R")

source("hill.r")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

run <- function(){

 
  y <- readline("Qual o periodo que pertende optimizar? \n[1] 18/11/1011 a 24/11/1011, [2] 25/11/2011 a 01/12/2011, [3] 02/12/2011 a 08/12/2011 \n")

  w <- readline("Qual o tipo de optimizacao? \n [1] Simulated Annealing, [2] climbing, [3] Montecarlo\n")

 
  y <- as.numeric(unlist(strsplit(y, ",")))

  w <- as.numeric(unlist(strsplit(w, ",")))

  
  n=0

	if(y==1){ n=14 }
	if(y==2){ n=7 }

	pred_ukDay(n)
	
	if(w==1){ simulated_Annealing(w)}
	if(w==2){ climbing(w)}
	if(w==3){ Funcmontecarlo(w)} 
	
#	if(x==4){
#	pred_daily_mat(n)
#	if(w==1){ sa_daily_mat(w,x,z) }
#	if(w==2){ mc_daily_mat(w,x,z) }
#	if(w==3){ sa_daily_mat(w,x,z); mc_daily_mat(w,x,z); graph_all() }
	#}
 # }
  #if(v==2){
	#if(y==1){ n=168 }
	#if(y==2){ n=84 }
	#if(x==1|x==2|x==3){
	#pred_hourly_vec(n,x)
	#if(w==1){ sa_hourly_vec(x,z)}
	#if(w==2){ mc_hourly_vec(x,z)}
	#if(w==3){ sa_hourly_vec(x,z); mc_hourly_vec(x,z)} 
	#}
	#if(x==4){
	#pred_hourly_mat(n)
	#if(w==1){ sa_hourly_mat(z) }
	#if(w==2){ mc_hourly_mat(z) }
	#if(w==3){ sa_hourly_mat(z); mc_hourly_mat(z) }
	#}
  #}
}






