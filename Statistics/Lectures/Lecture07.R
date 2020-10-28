###############################################
#Interpretation by repeated sampling
##############################################
a<-rep(0,100)
b<-a
c<-1.96/sqrt(500)
for(i in 1:100){
x<-rexp(500,5) #mu=0.2, n=500
a[i]<-mean(x)-c*sd(x) #xbar-z s/sqrt(n)
b[i]<-mean(x)+c*sd(x)}
length(which(a>.2|b<.2))/100
plot(a,ylim=c(.15,.25),type="n")
for(i in 1:100){lines(c(i,i),c(a[i],b[i]))}
abline(.2,0)
index<-which(a>.2|b<.2)
cbind(a[index],b[index])
#################################################
#Function
################################################
ciz = function(x, alpha=0.05){
z = qnorm( 1-alpha/2)
sdx = sd(x)/sqrt(length(x))
c(mean(x) - z*sdx, mean(x) + z*sdx)
}
#Demonstrate that the CI grows wider as confidence level increases.
cit= function(x, alpha=0.05){
  n = length(x)
  t = qt( 1-alpha/2,n-1 )
  sdx = sd(x)/sqrt(n)
  c(mean(x) - t*sdx, mean(x) + t*sdx)
}
# t interval is longer than z interval.
###################################################
#Bootstrap Interval
###########################################
cib<-function(x,alpha=0.05){
y<-rep(0,1000)
n<-length(x)
n1<-1000*alpha/2
n2<-1000-n1
for(i in 1:1000){
   u<-sample(x,n,replace=T)
   y[i]<-mean(u)}
y<-sort(y)
c(y[n1],y[n2])}
#No assumption about population
##############################################