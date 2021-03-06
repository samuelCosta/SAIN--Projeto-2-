source("hill.r")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
Funcmontecarlo = function(w)
{
print("Montecarlo")
TS=read.table("pred.ts",header=TRUE,sep=",")

eval=function(x) -profit(x)

Pm = round(TS,0)
o=Pm$x
custos =matrix(ncol=7,nrow=4);

#o = c(12063,12668,11563,13435,8447,0,3314) # orders

v2m=function(x,days=7)
{ m=matrix(x,nrow=days)
  return(t(m))
}

# x=sample(0:20,4*7,replace=TRUE)


if(FALSE){
h=c(6,14,10,2,8,6,1)
m1=c(1,12,2,1,9,3,0)
m2=c(4,7,5,5,1,0,1)
m3=c(0,2,0,0,0,1,0)
x <- do.call(rbind,list(h,m1,m2,m3))
}

profit=function(x) 
{ 
  c=cost(x)
  s=sales(x)
  profit=sum(s - c)
  return(profit)
}

cost=function(x)
{
 x=v2m(x)
 custos[1,]= x[1,]*350
 custos[2,]= x[2,]*500
 custos[3,]= x[3,]*1000
 custos[4,]= x[4,]*1540
  
  total = sum(custos)
  #print(custos)
 
  #cat("total cust: ",total,"\n")
  
  return (total)
}

sales= function(x)
{
  x=v2m(x)
  custos[1,]= x[1,]*5500
  custos[2,]= x[2,]*1000
  custos[3,]= x[3,]*2150
  custos[4,]= x[4,]*3400
  
 # print(custos)
  
  total1=sum(custos)
  
  #return(total1)
  mprod<- do.call(rbind, list(custos[2,],custos[3,],custos[4,]))

  #cria lista pprod
  pprod=matrix(ncol=7,nrow=1)

  #cria lista valores possible prod
  for (i in 1:7){ pprod[1,i]=sum(mprod[,i]) }

  #cat("possible prod: ", pprod,"\n")

  #cria lista psales
  psales=matrix(ncol=7,nrow=1)
  o <- do.call(rbind,list(o))
	
  #calcula valores possible sales
  for (i in 1:7){ if((o[,i])>(pprod[,i])) psales[1,i]=pprod[,i] else psales[1,i]=o[,i]}
  
  #cat("possible sales: ",psales,"\n")

  #cria lista dsales
  dsales=matrix(ncol=7,nrow=1)

  #calcula daily sales
  dsales=psales*2
  
  #cat("Daily sales: ",dsales,"\n \n")

  #calcula total sales
  totalsales = sum(dsales)
  
  #cat("Total sales",totalsales,"\n")
  
  
  return(totalsales)
}


#montecarlo
# dimension
D=7



N=1 # number of searches

lower=rep(0,D*4)
upper=rep(7,D*4)

MC=mcsearch(N=N,lower=lower,upper=upper,FUN=eval,type="min")
cat(">> Best Solution:","\n","Plano de Producao","\n","         S T Q Q S S D","\n","humans: ",round(MC$sol[1:7]),"\n","m 1:    ",round(MC$sol[8:14]),"\n","m 2:    ",round(MC$sol[15:21]),"\n","m 3:    ",round(MC$sol[22:28]),"\n","profit",MC$eval,"\n")

}

