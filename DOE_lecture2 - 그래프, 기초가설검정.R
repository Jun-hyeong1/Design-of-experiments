#####################################################################
#####################################################################
# Chapter 2   # stripchart(), boxplot(), #rnorm, dnorm, qnorm   #####
              # library(BSDA) -> z.test, t.test                 #####
              # library(EnvStats)-> var.test                    #####
#####################################################################
#####################################################################
stripchart(list('Unmodified'=data$y2, 'Modified'=data$y1))              #list기억!
abline(v=c(mean(data$y2), mean(data$y1)))


boxplot(list('Modified'=y1, 'Unmodified'=y2))


z.test(y1, y2, sigma.x=0.3, sigma.y=0.3)            


t.test(y1, y2, alternative="two.sided", var.equal=F)    #이상 평균이 같은가?


var.test(y1, y2, alternative = "less")    #분산이 같은가? 


##############################################################################
##############################################################################
rm(list=ls())

getwd()


data <- read.table("example_2_1.txt", sep="\t", header = T) 
data

# --- Graphical View of the Data ---
# 1) Dot Diagram
# stripchart : plot 형태의 그래프를 그리는 가장 기본적인 형태
# 데이터 수가 많지 않을 경우 사용하여 데이터의 분포를 확인할 수 있으며
# 박스 형태로 표시된 각 데이터 포인트가 있는 선을 따라 데이터를 순서대로 그림.

stripchart(list('Unmodified'=data$y2, 'Modified'=data$y1), 
           ylim=c(1,3), pch=16, xlab=expression(Strength(kgf/cm^2)))



axis(3, at=seq(16.38,17.36,by=0.14), lwd=1.5) # lwd : 선 두께.
# 앞의 1 : 좌표축 그릴 위치.
axis(1, at=seq(16.38,17.36,by=0.14), lwd=1.5, pos=1.9) # pos : 좌표축을 plot region에 나타낼 위치.

# 2) Box Plots
y1 <- data$y1  # Modified
y2 <- data$y2  # Unmodified

boxplot(list('Modified'=y1, 'Unmodified'=y2), xlab="Mortal formulation", 
        ylab=expression(Strength(kgf/cm^2)), yaxt="n")
# yaxt='n' : y축만 선별적으로 제거.
axis(2, at=seq(16.50,17.5,by=0.25))


# --- Hypothesis Testing Framework ---
# 1) Estimates the population mean 
y1_bar = mean(y1); y1_bar
y2_bar = mean(y2); y2_bar

n1 <- length(y1) # 개수.
n2 <- length(y2)

sum(y1)/n1; y1_bar #sum(y)/n = mean(y)
sum(y2)/n2; y2_bar

# 2) Estimates the population variance
y1_sig2 = var(y1); y1_sig2 
y2_sig2 = var(y2); y2_sig2

sum((y1 - y1_bar)^2)/(n1-1); y1_sig2 #(1/n-1)*sum(y-ybar)^2 = var(y)
sum((y2 - y2_bar)^2)/(n2-1); y2_sig2

# 3) Estimates the population standard deviation
y1_sig = sd(y1); y1_sig
y2_sig = sd(y2); y2_sig

sqrt(y1_sig2); y1_sig # root(var(y)) = sd(y)
sqrt(y2_sig2); y2_sig


# --- Probability distribution ---
# 1) Generate random numbers from such distribution
set.seed(1) # 결과가 똑같이 나올 수 있도록 seed 설정.

# 평균이 0이고, 표준편차가 1인 랜덤 넘버 10000개 생성
x <- rnorm(n=10000, mean=0, sd=1); x

# 2) Density function
dnorm(0, mean=0, sd=1) # x값, 평균 : 0, 표준편차 : 1
1/sqrt(2*pi)*exp(-(0)^2/2)

# -4부터 4 사이의 숫자 1000개
# length.out : 수열의 길이를 조절 1000개로 조정
# 공차는 수열의 길이에 따라 자동으로 설정

x2 <- seq(-4,4,length.out=1000)
plot(x2, dnorm(x2, mean=0, sd=1), type="l", main = "X~Normal(0,1)")
lines(density(x),col='red')

# 3) Cumulative Density function
pnorm(1.96,mean=0,sd=1) # 1.96까지 누적 확률을 계산.

# 4) Quantile function
qnorm(0.975,mean=0,sd=1)
qnorm(0.025,mean=0,sd=1)


# --- 1. If the variances are known ---
# Test statistics: Z0 (two-side test)
# If sigma = sigma1 = sigma2 = 0.3
z0 = (y1_bar - y2_bar) / sqrt(0.3^2 * (1/n1 + 1/n2)); z0

qnorm(0.025) # critical value of alpha = 0.05
2*pnorm(z0)  # Reject H0 under alpha = 0.05

# Using BSDA Package
# install.packages("BSDA")
library(BSDA)
z.test(y1, y2, sigma.x=0.3, sigma.y=0.3)

?z.test

# --- 2. If the variances are unknown ---
# 1) Case 1: n > 30 
#  => Sample sizes were large enough => Z-test
z0 = (y1_bar-y2_bar) / sqrt(y1_sig2/n1 + y2_sig2/n2); z0

qnorm(0.025) ## critical value of alpha = 0.05
2*pnorm(z0) ## Reject H0 under alpha = 0.05

# 2-1) Case 2-1:  n < 30 (Sample sizes were small) & sigma1 == sigma2 
#  => t-test using pooled estimator
n =  n1 + n2 - 2
s_pool2 = ((n1-1)*y1_sig2 + (n2-1)*y2_sig2) / n 

t0 = (y1_bar-y2_bar) / (sqrt(s_pool2) * sqrt(1/n1 + 1/n2)); t0

qt(0.025,n)
2*pt(t0,n)

t_res <- t.test(y1, y2, alternative = "two.sided", var.equal=T) 
t_res

t_res$statistic   # t statistic
t_res$conf.int[1:2] # 95% confidence interval
# 0이 포함되지 않음. -> H0 기각.
t_res$p.value # p-value

# 95% confidence interval
# (추정량 - t값 * 추정된 표준오차, 추정량 + t값 * 추정된 표준오차)
diff = y1_bar-y2_bar
t = qt(0.025, df = n, lower.tail=F); t
interval = t * sqrt(s_pool2) * sqrt(1/n1 + 1/n2)
c(diff - interval, diff + interval) 

# 2-2) Case 2-2:  n < 30 (Sample sizes were small) & sigma1 != sigma2 
t0 = (y1_bar-y2_bar) / sqrt(y1_sig2/n1 + y2_sig2/n2); t0
v = ((y1_sig2/n1) + (y2_sig2/n2))^2 / 
  (((y1_sig2/n1)^2 / (n1-1)) + ((y2_sig2/n2)^2 / (n2-1))); v

t_res2 <- t.test(y1, y2, alternative="two.sided", var.equal=F); t_res2
t_res2$parameter # d.f.

# --- Tests on Variance of Normal Distribution ---
# 1) Case 1: One-sample test on variance of Normal Distribution
# H0 : sigma1^2 = 0.15 vs H1 : sigma1^2 < 0.15
chisq0 = ((n1-1) * y1_sig2) / 0.15; chisq0

#install.packages("EnvStats")
library(EnvStats)
res = varTest(y1, alternative="less", sigma.squared=0.15); res
res$statistic
res$p.value

# H0 : sigma2^2 = 0.05 vs H1 : sigma2^2 > 0.05
varTest(y2, alternative="greater", sigma.squared=0.05)

# 2) Case 2: Two-Sample test on variance of Normal Distribution
# H0 : sigma1^2 = sigma2^2 vs H1 : sigma1^2 != sigma2^2  
f0 = y1_sig2/y2_sig2; f0
var.test(y1, y2, alternative = "two.sided")



