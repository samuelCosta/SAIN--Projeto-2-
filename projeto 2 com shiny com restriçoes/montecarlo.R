### montecarlo.R file ###

# montecarlo uniform search method
#    N - number of samples
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    domain - vector list of size D with domain values
#    FUN - evaluation function
#    type - "min" or "max"
#    ... - extra parameters for FUN
mcsearch=function(N,lower,upper,FUN,type="min",...)
{ 
  D=length(lower)
  s=matrix(nrow=N,ncol=D) # set the search space 
  for(i in 1:N) s[i,]=f1() #s[i,]=runif(D,lower,upper)
  fsearch(s,FUN,type,...) # best solution
}

f1=function(){
D=7
lower=rep(0,D)
upper=rep(7,D)
h=runif(D,lower,upper)
m1=runif(D,lower,upper)
m2=runif(D,lower,upper)
m3=runif(D,lower,upper)

for (i in 1:D){
while(h[i]>0 && h[i]<sum(m1[i],m2[i],m3[i])) {
m1[i]<-runif(D,lower,upper)
m2[i]<-runif(D,lower,upper)
m3[i]<-runif(D,lower,upper)
}}
v= c(h,m1,m2,m3)
return(v)
}