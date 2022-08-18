rm(list=ls())
getwd()

#####################################################################
#####################################################################
# Chapter 5 
#
#####################################################################
#####################################################################


# Example 5.3 
#세개의 팩터와 반복실험 n번 

# a level : 3 / b level : 2 / c level : 2 / n : 2

carb = factor(c(10,12,14)) # factor A
psi = factor(c(25,30)) # factor B
speed = factor(c(200,250)) # factor C
times = factor(c(1,2)) # n

dat_5_3 = expand.grid(times=times,speed=speed,    #times부터 적는게 좋다!
                      psi=psi,carb=carb)

dim(dat_5_3)


dat_5_3$value<-c(-3,-1,-1,0,-1,0,1,1,
                 0,1,2,1,2,3,6,5,
                 5,4,7,6,7,9,10,11) 

# ANOVA 모델 
fit_5_3<- aov(value~carb*psi*speed,data=dat_5_3)
summary(fit_5_3)
anova(fit_5_3)

# Main effects for factor A,B,C
dat_A = tapply(dat_5_3$value,dat_5_3$carb,mean)
dat_B = tapply(dat_5_3$value,dat_5_3$psi,mean)
dat_C = tapply(dat_5_3$value,dat_5_3$speed,mean)

par(mfrow=c(1,1))


#factor A Main effect plot 
plot(dat_A,type='b',col='blue',xaxt='n',
     main = 'Main effects for Percent carbonation (A)')

axis(1,at=c(1,2,3),labels=c(10,12,14),lwd.ticks = 2)



plot(dat_B,type='b',col='blue',xaxt='n',
     xlim=c(0,3),ylim=c(0,5))
axis(1,at=c(1,2),labels=c(25,30),lwd.ticks = 2)

# AB interaction plot.
interaction.plot(dat_5_3$carb,dat_5_3$psi,
                 response=dat_5_3$value,
                 ylim=c(-2,10),xaxt='n',
                 xlab = 'Carbonation-pressure interaction',
                 ylab = 'Response',
                 main = 'Carbonation-pressure Interaction',
                 legend=F,col=c('blue','red'))
axis(1,at=c(1,2,3),labels=c('10','12','14'),lwd.ticks = 2)
legend('topleft',c('25 psi','30 psi'),
       lty=2:1,col=c('blue','red'))

# Factorial Eperiments (part2)##################################################
# Factor A : Mtype, Factor B : temperature, times = 4(n)
temp = factor(c(15,70,125)) 
Mtype = factor(1:3) 
times = factor(1:4) 


dat_5_4 = expand.grid(times=times,temp=temp,Mtype=Mtype)
dim(dat_5_4) 

dat_5_4$value<-c(130,155,74,180, 34,40,80,75, 20,70,82,58,
                 150,188,159,126, 136,122,106,115, 25,70,58,45,
                 138,110,168,160,174,120,150,139,96,104,82,60)

# 두 팩터에 대한 ANOVA

fit_origin = aov(value~Mtype * temp, data=dat_5_4)
summary(fit_origin)

str(dat_5_4)

# 여기서 temp를 실수 타입으로 변환

dat_5_4$temp<-as.numeric(as.character(dat_5_4$temp))
str(dat_5_4)

# ANOVA                                         # temp^2을 포함하는 ANOVA

fit_5_4<-aov(value~temp*Mtype+Mtype*I(temp^2),data=dat_5_4)
summary(fit_5_4)

# aov(value~temp*Mtype)
# temp(A)*Mtype(B) -> A,B,AB
# Mtype*I(temp^2) -> I(temp^2)=A^2, A^2B


# (regression)               #회귀 모형 적합  (전체 자료에대해)

fit1 = lm(value ~ temp+I(temp^2),data=dat_5_4,
          subset=Mtype==1)
summary(fit1)
predict(fit1)



# (regression)             #회귀모형 적합  (각 재료에 대해 3번)

fit_mtype = list()

for(i in 1:3){
  fit_mtype[[i]] = lm(value~temp+I(temp^2),
                      data=dat_5_4,subset=Mtype==i)
  print(summary(fit_mtype[[i]]))}

# plot.
par(mfrow=c(1,1))

plot(dat_5_4$temp,dat_5_4$value,
     
     pch=16,
     ylim=c(min(dat_5_4$value),max(dat_5_4$value)),
     xaxt='n',yaxt='n',ylab='Life',
     xlab='Temperature')
axis(1,seq(15,125,by=27.5))
axis(2,seq(20,188,by=42))

for(i in 1:3){
  lines(c(15:125),predict(fit_mtype[[i]],
                            data.frame(temp=c(15:125))))
}

text(locator(1),"Material type 1")        #실행하고 나서 꼭 클릭해주기
text(locator(1),"Material type 2")
text(locator(1),"Material type 3")

