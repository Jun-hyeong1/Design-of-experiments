
#####################################################################
#####################################################################
# Chapter 3   # tapply(data$etch, data$power, sum)                  #####
#             #       (대상)을. power값으로 sum,  y_idot을 구한다.                                             
# 
#
#qqnorm(fit_etch$residuals)   잔차의 정규성검정, 가정이 타당한가.   ##### 
#qqline(fit_etch$residuals)


#plot(fit_etch) 모델 검증에 대한 plot들


#shapiro.test(fit_etch$residuals) 정규성 검정 함수                  #####



#library(lawstat)->levene.test(data$etch, data$power, location="mean")
#등분산 검정 함수                                                   #####


#####################################################################
#####################################################################



getwd()
## removes all objects
rm(list=ls())

## Example 3.1
power <- factor(c(160,180,200,220)); power
times <- factor(c(1,2,3,4,5)); times

# expand.grid : 가능한 모든 factor 레벨의 조합을 만든다. #(리스트 생성)
# 여기서는 power와 time의 모든 가능한 조합은 20개이다.
data <- expand.grid(times=times, power=power)
data


etch_rate<- c(575,542,530,539,570,
              565,593,590,579,610,
              600,651,610,637,629,
              725,700,715,685,710)

data$etch <- etch_rate
data


## Method 1 
y_idot <- rep(0,4)
y_idotbar <- rep(0,4)

for(i in 1:4){
  y_idot[i] = sum(data$etch[data$power==levels(data$power)[i]])
  y_idotbar[i] = mean(data$etch[data$power==levels(data$power)[i]])
}


y_idot
y_idotbar


## Method 2
# tapply는 그룹별로 값을 구할 때 많이 사용합니다.

y_idot2 <- tapply(data$etch, data$power, sum)
y_idot2

y_idotbar2 <- tapply(data$etch, data$power,function(x) mean(x))
y_idotbar2


## Box plot & Scatter plot
# 여러 개의 그림을 그리고 싶을 때 par(mfrow=c()) 이 함수를 많이 사용.
# 아래의 par(mfrow=c(1,2))는
# c안의 값에서 앞의 1은 행을 의미하고, 뒤의 2는 열을 의미하여
# 1행 2열 즉 2개의 그림을 보여줍니다.

par(mfrow=c(1,2))

# Box plot
plot(data$etch ~ data$power,
     main = "Example 3.1 by boxplot",
     xlab = "Power", ylab = "Etch-rate")

# Scatter plot
plot(data$etch ~ as.character(data$power),
     main = "Example 3.1 by scatter plot", 
     xlab = "Power", ylab = "Etch-rate")


## One-way ANOVA
# 분산분석 ANOVA를 보고 싶을 때는 aov 함수를 사용.

fit_etch <- aov(etch ~ power, data=data)
anova(fit_etch)
summary(fit_etch)

fit_etch2 <- lm(etch ~ power, data=data) 
## It is same as aov function if treatment's(power) value type is factor.
anova(fit_etch2)
summary(fit_etch2)

fit_etch3 <- lm(etch ~ as.numeric(as.character(power)), data=data)
anova(fit_etch3)
summary(fit_etch3)

## Sum of Square
SS_vec <- anova(fit_etch)$'Sum Sq'; SS_vec

SS_total <- sum(SS_vec) # sum(yij)
SS_treat <- SS_vec[1] # SStreatement
SS_error <- SS_vec[2] # SSE
SS_total; SS_treat; SS_error


## Checking normal assumption
# Method 1
# 정규성 검정.
par(mfrow=c(1,1))
qqnorm(fit_etch$residuals)
qqline(fit_etch$residuals)

# Method 2
par(mfrow=c(2,2))
plot(fit_etch)

# Method 3
# shapiro test
# H0 : 귀무가설은 데이터의 분포가 정규분포를 따른다.
# H1 : 대립가설은 데이터가 정규분포를 따르지 않는다 입니다.
shapiro.test(fit_etch$residuals)

## Plot of residuals versus run order or time    
set.seed(1993)
spl_idx <- sample(1:20, 20) 
spl_idx

fit_etch$residuals[spl_idx]                #인덱스를 랜덤화해서 plot그리기 
par(mfrow=c(1,1))
plot(fit_etch$residuals[spl_idx],
     main = "Plot of resiudals vs run order or time", 
     ylab = "residual", xlab = "Run order")
abline(h=0, col="red")

# Constant variance
# 집단간 분산이 같은지 다른지 여부를 알아볼 때 사용.
# 표본이 정규성을 보이지 않아도 사용할 수 있음.

# install.packages("lawstat")
library(lawstat)

# H0 : The k group variances are equal.
# H1 : The k group variances are not equal.
# location : 어떤 값을 기준으로 검정.
# 아래는 평균으로 검정.
levene.test(data$etch, data$power, location="mean")

## Numeric Power Regression models
data$num_power <- as.numeric(as.character(data$power))

fit_reg <- lm(etch ~ num_power,data=data)
plot(data$etch ~ data$num_power,
     main = "Example 3.1 for Regression",
     xlab = "Power", ylab = "Etch-rate")
abline(fit_reg)

fit_reg3 <- lm(etch ~ num_power + I(num_power^2),data=data)
lines(data$num_power,y=fit_reg3$fitted.values, type="l", col = "red")
legend("topleft", legend = c("Linear","Quadratic"),
       col=c("black","red"), lty=c(1,1))

