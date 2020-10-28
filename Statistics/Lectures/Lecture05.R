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
p<-0.5
b<-rep(0,11)
n<-b
for (i in 1:11) b[i]<-dbinom(i-1,10,p) #X~Bin(10,.1) P(X=i)=b[i],i=0,...10
m<-10*p  #m=EX
s<-sqrt(10*p*(1-p)) #s=SD(X)
for (i in 1:11)n[i]<-pnorm((i-.5-m)/s)-pnorm((i-1.5-m)/s)  #[0,.5],[.5,1.5],...[9.5,10.5]
max(abs(b-n))
barplot(rbind(n[1:11],b[1:11]),beside=T,names.arg=c(0:4))
###############################################
#SLLN
##########################################
runningmean = function (x,N){
 y = sample(x,N, replace=TRUE)
 c = cumsum(y) #c[i]=sum(y[1],...y[i])
 n = 1:N #n[i]=i
 c/n
 }
u = runningmean(c(0,1), 1000)
x=1:1000; plot(u~x, type="l");
replicate(10, lines(runningmean(c(0,1), 1000)~x, type="l", col=rgb(runif(3),runif(3),runif(3))))
###########################################
#CLT
##########################################
s<-rep(0,8)
for(i in 1:8){
m <- combn(10, i,mean) #without replacement
s[i]<-sd(m)}
plot(1/s^2)

#var(x bar)=sigma^2/n

s<-rep(0,8)
for(i in 1:8){
m <- combn(10, i,mean)
s[i]<-3*sd(m)/sqrt(10-i)} #corrected for small sample
plot(1/s^2)

n<-1000
p<-0.01
x<-rbinom(1000,n,p)/n; qqnorm(x); abline(p,sqrt(p*(1-p)/n)) #binomial proportion
############################################################
## 4.3 Example from statlab
## https://www.stat.berkeley.edu/~statlabs/about.html
############################################################
babies<-read.table("https://www.stat.berkeley.edu/~statlabs/data/babies.data",header=TRUE)
attach(babies)
#The 999 values in bwt denote missing values. To replace 999 with NA.
replace(bwt,bwt==999,NA)
replace(gestation,gestation==999,NA)
gestation[which(gestation==999)]<-NA
weight[which(weight==999)]<-NA
#The 9 values in smoke denote missing values. To replace 9 with NA and recode 2 and 3 as 0, for nonsmoker:
  ismoke<- replace(smoke,smoke==2 |smoke==3,0)
ismoke[ismoke==9]<-NA
#To compute the average birthweight for smokers and nonsmokers.
tapply(bwt,ismoke,mean)
#To select babies whose mother smoked.
smokerbabes<-babies[is.na(ismoke) | ismoke==1,]
#To count babies according to whether they are premature or low birthweight:
  table(cut(gestation,4),bwt<90)
#To make a quantile-quantile plot of bwt for smokers versus nonsmokers
qqplot(bwt[ismoke==0],bwt[ismoke==1])
abline(0,1)
#To make a gamma(5,1) quantile plot of bwt
ps<-ppoints(length(bwt))
plot(quantile(bwt,ps),qgamma(ps,5,1))
#To put box and whisker plots of bwt, one for each smoking level, on the same plot
boxplot(bwt~ismoke)
################################################
#Shuffling cards
###############################################
x<-paste(c(rep("Clubs",13),rep("Diamonds",13),rep("Hearts",13),rep("Spades",13)),rep(c(seq(2:10),"Jack","Queen","King","Ace"),4),sep="-")
x[sample(1:52)]
####################################################
# 8 perfect shuffles will restore the deck
#######################################
x<-1:52
x
for(i in 1:26){x[i]<-i*2-1}  #position of ith card
for(i in 27:52){x[i]<-(i-26)*2}
x[x[x[x[x[x[x[x]]]]]]]
2^8%%51 #1
# 5 9 17 33 14 27 2 3 5 Card 5 goes to 9th position, then to 17th and so on.
# Diaconis, Persi, R. L. Graham, and William M. Kantor. "The mathematics of perfect shuffles." Advances in applied mathematics 4.2 (1983): 175-196.