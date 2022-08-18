rm(list =ls())

getwd()


##############                                                  
########2^2 디자인 

A<-c(-1,1)
B<-c(-1,1)
times<-1:3

data<-expand.grid(times=times,A=A,B=B )
data

data$value<- c(28,25,27,
               36,32,32,
               18,19,23,
               31,30,29)
fit<-aov(value~A*B, data)

anova(fit)



#콘트라스트를 생성한다. 

cont<-model.matrix(~ -1+A*B, data)   #-1은 intercept 제거를 위해
cont

#effect 추정(eff_A= (contrast_A)/(2*n))
eff_vec =data$value%*%cont/(2*3)
eff_vec

#SS추정 
ss_vec<- (data$value%*%cont)^2/(4*3)
ss_vec
#anova 테이블이랑 같은 값이다. 


#회귀 모형 적합

fit_lm<-lm(value~A+B, data)
summary(fit_lm)




#반응표면을 만들어보자. 

#install.packages("rsm")
library(rsm)
??rsm

persp(fit_lm, B~A, zlim=c(15,35))


#컨투어 플롯을 그려보자 

contour(fit_lm,B~A, level=seq(23,33,2))




####################
#2^3 디자인    #####  위와 거의 같다. 
####################
####################

A<-c(-1,1)
B<-c(-1,1)
C<-c(-1,1)
times<-1:2

data<-expand.grid(times=times,A=A,B=B, C=C)
data

data$value<- c(550,604,
               669,650,
               633,601,
               642,635,
               1037,1052,
               749,868,
               1075,1063,
               729,860)

fit<-aov(value~A*B*C, data)

anova(fit)



fit_reduced<-aov(value~A*C, data)

anova(fit_reduced)



#추정

cont<-model.matrix(~ -1+A*B*C, data)

eff_vec =data$value%*%cont/(2^3)

ss_vec<- (data$value%*%cont)^2/(2^4)






#(회귀)

fit_lm<-lm(value~A+C, data)
summary(fit_lm)
eff_vec/2  #와 같다. 

#install.packages("qpcR")
library(qpcR)


#####PRESS(fit_lm)??????


persp(fit_lm, C~A, zlim=c(500,1057))

contour(fit_lm,C~A, level=seq(673.625,980.125,length.out=5))   #줄 다섯개를 그려라 
contour(fit_lm,C~A)










