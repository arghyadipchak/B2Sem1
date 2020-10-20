#########################################################
# Probability with R
##########################################################
#1 Random Sampling
##########################################################
#Pick 5 students from this class. Make alphabetical list. Simple Random Sampling Without Replacement (SRSWOR):
n<-47
sample(1:n,10)
#Toss a coin
sample(c("Head","Tail"),1)
#Roll a die
sample(1:6,1)
#Roll the die 10 times. Numbers can repeat. Simple Random Sample With Replacement (SRSWR):
sample(1:6,10,replace=T)
# Histogram
x = sample(1:6,100,replace=T)
y = hist(x,0.5+0:6)
#Pie chart
pie(y$counts)
##########################################################
#2 Problem 1a of probability 1 midterm
#10 balls labelled 1, 2, . . . , 10 are thrown uniformly at random into 4 
#labelled bins (i.e., balls are thrown one by one independently into bins 
#chosen uniformly at random). In each of the following cases, prove or 
#disprove that A and B are independent and also compute P(B|A) and P(B).
#(a) Let A be the event that the first bin contains an even numbered ball. 
#Let B be the event that the second bin contains an odd numbered ball.
############################################################
n<-1000000
A<-rep(0,n)
B<-A
for(i in 1:n){
  S<-sample(1:4,10, replace=T)
  A[i]<-(S[2]==1||S[4]==1||S[6]==1||S[8]==1||S[10]==1)
  B[i]<-(S[1]==2||S[3]==2||S[5]==2||S[7]==2||S[9]==2)}
(sum(A*B)*n)/(sum(A)*sum(B))
sum(B)/n
sum(A*B)/sum(A)
1-(3/4)^5  # 0.7626953
# No general result (needed to specify 10 and 4). This is not a proof.
###########################################################
#3 Discrete distributions
############################################################
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
rhyper(nn, m, n, k)
dpois(x, lambda, log = FALSE)
############################################################
#4 Poisson approximation to binomial 
##############################################################
p<-hist(rpois(10000,1),0.5+-1:8)
b<-hist(rbinom(10000,1000,0.001),0.5+-1:8)
(b$counts-p$counts)/10000
#[1]  0.0014 -0.0007  0.0008 -0.0022  0.0019 -0.0008 -0.0005  0.0001  0.0000
##############################################################
#5 Continuous distributions
#############################################################
dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
u<-runif(100000, min = 0, max = 1)
var(u)-1/12
#[1] 0.0002057412
mean(u)
pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)
dbeta(x, shape1, shape2, ncp = 0, log = FALSE)
pnorm(x)
qnorm(p)
dnorm(x)
rnorm(n, mu, sigma)
hist(rnorm(1000,0,1))
#################################################################
#Geometric as discrete exponential
###############################################
g<-dgeom(seq(1:20)-1,.2)
h<-pexp(seq(1:21)-.5,.2)
e<-h[-1]-h[1:20]
max(abs(e-g))
###################################################################
#Transformation from unif to beta
##################################################################
x<-runif(10000,0,1)
y<-x^2
s<-seq(0,1,by=.01)
hist(y,s,freq=FALSE)
f<-dbeta(s-.005,1/2,1)
lines(s[-1]-.005,f[-1],col="red")