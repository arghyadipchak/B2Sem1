a<-rep(0,100)
b<-a
c<-1.96/sqrt(500)
for(i in 1:100){
x<-rexp(500,5)
a[i]<-mean(x)-c*sd(x)
b[i]<-mean(x)+c*sd(x)}
length(which(a>.2|b<.2))/100