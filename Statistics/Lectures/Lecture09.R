
##############################################
#ANOVA using iris data
###############################################
a<-tapply(iris$Petal.Length,iris$Species,var)
b<-tapply(iris$Petal.Length,iris$Species,mean)
SSE<-sum(a)*49
c<-mean(iris$Petal.Length)
SST<-50*(b[1]-c)^2+50*(b[2]-c)^2+50*(b[3]-c)^2

149*var(iris$Petal.Length)-SSE

SSTotal=SSE+SST

aov(formula = Petal.Length ~ Species, data = iris)
ff<-(SST/(k-1))/(SSE/(n-k))
qf(.95,2,147)
n<-150
k<-3
############################################################
#Chisq test of goodness of fit using marijuana example from lecture
####################################################
x<-c(39,99,336,26)
chisq.test(x,p=c(0.07,0.18,0.65,0.1))
qchisq(.99,3)
#######################################################
#Chisq test of independence using HairEyeColor data
#########################################################
data("HairEyeColor")
HairEyeColor
chisq.test(HairEyeColor[,,"Female"]) #Independence of hair color and eye color for females
dim(HairEyeColor[,,"Female"])
chisq.test(HairEyeColor[,,"Male"])
chisq.test(HairEyeColor[,,"Male"]+HairEyeColor[,,"Female"]) #Both genders together
margin.table(HairEyeColor,c(1,3))
chisq.text(margin.table(HairEyeColor,c(1,3))) #Independence of hair color and gender
chisq.test(margin.table(HairEyeColor,c(2,3))) #Independence of eye color and gender