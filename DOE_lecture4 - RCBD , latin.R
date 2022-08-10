#####################################################################
#####################################################################
# Chapter 4   #  psi <- factor(c(8500, 8700, 8900, 9100)) 
#             #  data<- expand.grid(block = block, psi = psi)                                         
# 
#fit_rcbd <- aov(value ~ psi + block, data = dat_4_1)
#
#fit_latin <- aov(value ~ trt + block1 + block2, data = dat)
#
#####################################################################
#####################################################################

#plot(Resid ~ as.character(dat_4_1$psi))  산점도
#plot(Resid ~ (dat_4_1$psi))     박스플랏       # 이두개는 신기하게 다르다. 


rm(list=ls())

# example 4.1 (create data set(example 4.1))
psi <- factor(c(8500, 8700, 8900, 9100))
block = factor(1:6)

a <- length(psi)
b <- length(block)

# expand.grid : 가능한 모든 factor 레벨의 조합을 만든다.
dat_4_1 = expand.grid(block = block, psi = psi)
dim(dat_4_1)

dat_4_1$value <- c(90.3, 89.2, 98.2, 93.9, 87.4, 97.9,
                   92.5, 89.5, 90.6, 94.7, 87, 95.8,
                   85.5, 90.8, 89.6, 86.2, 88, 93.4,
                   82.5, 89.5, 85.6, 87.4, 78.9, 90.7)
head(dat_4_1)

# compute sum of row and column using tapply
# tapply를 사용해서 group별 합 확인하기.
?tapply
block_sum <- tapply(dat_4_1$value, dat_4_1$block, sum) #y.j
trt_sum <- tapply(dat_4_1$value, dat_4_1$psi, sum) #yi.
total_y <- sum(dat_4_1$value) # y..

# compute Sum of square
CT = total_y^2/(a*b)
ss_trt <- sum(trt_sum^2)/b - CT
ss_block <- sum(block_sum^2)/a - CT
ss_total <- sum(dat_4_1$value^2) - CT
ss_error <- ss_total - ss_trt - ss_block

# RCBD design
fit_rcbd <- aov(value ~ psi + block, data = dat_4_1)
summary(fit_rcbd)
anova(fit_rcbd)

# RCBD design vs CRD design
fit_CRD <- aov(value ~ psi, data = dat_4_1)
summary(fit_CRD)
anova(fit_CRD)

# model checking
x11()
par(mfrow = c(2,2))
plot(fit_rcbd)

# shapiro test
# H0 : 귀무가설은 데이터의 분포가 정규분포를 따른다.
# H1 : 대립가설은 데이터가 정규분포를 따르지 않는다 입니다.
shapiro.test(fit_rcbd$residuals)

# Some other plots
Resid = fit_rcbd$residuals ## resid(fit_rcbd)

par(mfrow=c(1,2))
plot(Resid ~ as.character(dat_4_1$psi), 
     xlab = "Treatments", ylab= "Residuals", 
     main = "Residuals by Treatments", pch = 4)
abline(h=0, col="red")

plot(Resid ~ as.character(dat_4_1$block), 
     xlab = "Blocks", ylab = "Residuals", 
     main = "Residuals by Blocks", pch = 2)
abline(h=0, col="blue")

# random eff
## compute sigma beta square estimation
anova_fit <- anova(fit_rcbd)
sigma_beta_2 <- (anova_fit$`Mean Sq`[2] - anova_fit$`Mean Sq`[3])/a


# latin square design
batch <- factor(1:5)
batch2 <- factor(1:5)

## Method 1 (Direct input)
trt <- c("A","B","C","D","E",
         "B","C","D","E","A",
         "C","D","E","A","B",
         "D","E","A","B","C",
         "E","A","B","C","D")

dat <- expand.grid(block1 = batch, block2 = batch2)
dat$trt <- trt
dat
head(dat)
dat$value <- c(24, 17, 18, 26, 22,
               20, 24, 38, 31, 30,
               19, 30, 26, 26, 20,
               24, 27, 27, 23, 29,
               24, 36, 21, 22, 31)
head(dat)

## Method 2 (input using for function[위로 올리는 방식])

### Method 2-1(dual for)
rp_fac = c("A","B","C","D","E")
rp = c(rp_fac,rp_fac[1:4])
design2 <- c()

for(i in 1:5){
  for(j in 1:5)
  {
    design2[5*(i-1)+j] = rp[i+j-1]
  }
}
design2 = as.factor(design2)
length(design2)

### Method 2-2(single for)
design3 = c()
for(i in 1:5){
  design3[5*(i-1)+1:5] = rp[1:5+i-1]
}
design3 = as.factor(design3)
design2
design2==design3

# fitting latin square design
fit_latin <- aov(value ~ trt + block1 + block2, data = dat)
summary(fit_latin)
anova(fit_latin)
