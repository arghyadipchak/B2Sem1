###########################################
#Assessing Normality
###############################################
ceo.compensation.2014 <- read.delim("C:/Users/jhump/Desktop/Newer/StatI/StatI/ceo-compensation-2014.txt")
View(ceo.compensation.2014)
attach(ceo.compensation.2014)
hist(log(Ratio))
x<-log(Ratio)
mean(x)
sd(x)
length(which(x<mean(x)+sd(x)&x>mean(x)-sd(x)))/length(x)
length(which(x<mean(x)+2*sd(x)&x>mean(x)-2*sd(x)))/length(x)
length(which(x<mean(x)+3*sd(x)&x>mean(x)-3*sd(x)))/length(x)
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)
qqnorm(x)
abline(mean(x),sd(x))
library(timeDate)
skewness(x)  # 0.08324829
kurtosis(x,method="moment") #3.873771
IQR(x)/sd(x)   #1.201651
#############################################
#Approximating binomial with normal
######################################
p<-0.1
b<-rep(0,11)
n<-b
for (i in 1:11) b[i]<-dbinom(i-1,10,p)
m<-10*p
s<-sqrt(10*p*(1-p))
for (i in 1:11)n[i]<-pnorm((i-.5-m)/s)-pnorm((i-1.5-m)/s)
max(abs(b-n))
barplot(rbind(n[1:5],b[1:5]),beside=T,names.arg=c(0:4))
###############################################
#SLLN
##########################################
runningmean = function (x,N){
 y = sample(x,N, replace=TRUE)
 c = cumsum(y)
 n = 1:N
 c/n
 }
u = runningmean(c(0,1), 1000)
x=1:1000; plot(u~x, type="l");
replicate(10, lines(runningmean(c(0,1), 1000)~x, type="l", col=rgb(runif(3),runif(3),runif(3))))
###########################################
#CLT
##########################################
S<-rep(0,9)
for(i in 1:9){
m <- combn(10, i,mean) #without replacement
s[i]<-sd(m)}
plot(s)

S<-rep(0,9)
for(i in 1:9){
m <- combn(10, i,mean)
s[i]<-sqrt(10-i)*sd(m)/3} #corrected for small sample
plot(s^2)

x<-rbinom(1000,n,p)/n; qqnorm(x); abline(p,sqrt(p*(1-p)/n)) #binomial proportion