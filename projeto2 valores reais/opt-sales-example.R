# --------- Product Sales example ----------
source("hill.r")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here


eval=function(x) -profit(x)

o = c(12063,12668,11563,13435,8447,0,3314) # orders
custos =matrix(ncol=7,nrow=4);
v2m=function(x,days=7)
{ m=matrix(x,nrow=days)
  return(t(m))
}
f1=function(){

h=sample(1:7,replace=TRUE)
m1=sample(0:6,replace=TRUE)
m2=sample(0:6,replace=TRUE)
m3=sample(0:6,replace=TRUE)

#print(h)
#print(m1)
#print(m2)
#print(m3)
#cat("\n")

for (i in 1:7){
while(h[i]<sum(m1[i],m2[i],m3[i])) {
m1[i]=sample(0:6,1,replace=TRUE)
m2[i]=sample(0:6,1,replace=TRUE)
m3[i]=sample(0:6,1,replace=TRUE)
}}
#print(h)
#print(m1)
#print(m2)
#print(m3)
v= c(h,m1,m2,m3)
return(v)
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
if(FALSE){
#climbing
D=7

#cat("Simulated Annealing (example with",Runs,"different seeds/runs): \n")
#x=sample(1:1000,Runs)
#y=sample(1:1000,Runs)

lower=rep(0,D*4)
upper=rep(7,D*4)
N=1000

rchange2=function(par) # change for hclimbing
	{ hchange(par,lower=lower,upper=upper,rnorm,mean=0.5,sd=0.5,round=TRUE) }


CSANN=list(maxit=N,temp=10,trace=TRUE)

s1=sample(1:7,4*D,replace=TRUE)
SA=optim(par=s1,fn=eval,method="SANN",gr=rchange2,control=CSANN)
#cat("best solution:",SA$par,"evaluation function",SA$value,"\n")

cat(">> Best Solution (Climbing):","\n","Plano de Producao","\n","         S T Q Q S S D","\n","humans: ",round(SA$par[1:7]),"\n","m 1:    ",round(SA$par[8:14]),"\n","m 2:    ",round(SA$par[15:21]),"\n","m 3:    ",round(SA$par[22:28]),"\n",
"profit:",SA$value,"\n")
}

#------------------------------------------------------------------------------
D=7

# simulated annealing:
Runs=7

cat("Simulated Annealing (example with",Runs,"different seeds/runs): \n")
lower=rep(0,D*4)
upper=rep(7,D*4)


rchange2=function(par) # change for hclimbing
	{ hchange(par,lower=lower,upper=upper,rnorm,mean=0.0,sd=0.0,round=TRUE) }

best= -Inf # - infinity
for(i in 1:Runs)
{
s1=f1()
#s1=sample(1:7,4*D,replace=TRUE)
print(s1)
 sa= optim(par=s1,fn=eval,method="SANN",gr=rchange2,control=list(maxit=6000, temp=2000, trace=FALSE))
 L=profit(sa$par)
 cat("execution:",i," solution:",sa$par," profit:",L,"\n")
 if(L>best) { BESTSA=sa; best=L;}
}
cat(">> Best Solution (Simulated Annealing):","\n","Plano de Producao","\n","         S T Q Q S S D","\n","humans: ",round(BESTSA$par[1:7]),"\n","m 1:    ",round(BESTSA$par[8:14]),"\n","m 2:    ",round(BESTSA$par[15:21]),"\n","m 3:    ",round(BESTSA$par[22:28]),"\n",
"profit:",profit(BESTSA$par),"\n","costs:",cost(BESTSA$par)," sales:",sales(BESTSA$par),"\n")


#------------------------------------------------------------------------------------

if(FALSE){
#montecarlo
# dimension
D=7



N=1000 # number of searches

lower=rep(1,D*4)
upper=rep(7,D*4)

MC=mcsearch(N=N,lower=lower,upper=upper,FUN=eval,type="min")
#cat("best solution:",MC$sol,"evaluation function",MC$eval,"\n")
cat(">> Best Solution (Montecarlo):","\n","Plano de Producao","\n","         S T Q Q S S D","\n","humans: ",round(MC$sol[1:7]),"\n","m 1:    ",round(MC$sol[8:14]),"\n","m 2:    ",round(MC$sol[15:21]),"\n","m 3:    ",round(MC$sol[22:28]),"\n",
"profit:",MC$eval,"\n")


}