#####################################################################
#####################################################################
# Chapter 5   #  
#fit_fac3 <- aov(value ~ mtype + temp + mtype*temp, data = dat_5_3)  #two factor model
#anova(fit_fac3)
#
#interaction.plot(dat_5_3$temp, dat_5_3$mtype, response = dat_5_3$value)
#
#                  #factor 1      #factor 2    #response = value             
#
#####################################################################
#####################################################################


rm(list=ls())

# ch 5 Factorial experiments
# 5.1 basic definitions and principles

A <- factor(0:1)
B <- factor(0:1)

dat <- expand.grid(A=A, B=B); dat;
dat$value <- c(20,40,30,52)
dat

# 1) Main effect
eff_A <- mean(dat$value[dat$A == 1]) - mean(dat$value[dat$A == 0])
eff_B <- mean(dat$value[dat$B == 1]) - mean(dat$value[dat$B == 0])

# 2) Interaction effect
eff_AB <- mean(dat$value[dat$A == dat$B]) - mean(dat$value[dat$A != dat$B])

c(eff_A,eff_B,eff_AB)

# Interaction plot
interaction.plot(dat$A, dat$B, response = dat$value,
                 ylim = c(10,60), xaxt = "n",
                 xlab = "Factor A", ylab = "response",
                 main = "Interaction plot in figure 5.3",
                 legend = F, col = c("blue","red"))

axis(1, at = c(1,2), labels = c("-","+"), lwd.ticks = 3)
legend("topleft", c("B-","B+"), lty = 2:1, col = c("blue","red"))

#------------------------------------------------------------------------------------

# 5.3 Two-factor factorial design
mtype <- factor(1:3)
temp <- factor(c(15, 70, 125))
times <- 1:4

dat_5_3 <- expand.grid(times = times, temp = temp, mtype = mtype)
dim(dat_5_3)
dat_5_3$value <- c(130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58,
                   150, 188, 159, 126,136, 122, 106, 115, 25, 70, 58, 45,
                   138, 110, 168, 160,174, 120, 150, 139, 96, 104, 82, 60)
head(dat_5_3)

# method 1
fit_fac <- aov(value ~ mtype + temp + mtype:temp, data = dat_5_3)
anova(fit_fac)

# method 2
fit_fac2 <- aov(value ~ mtype*temp, data = dat_5_3)
anova(fit_fac2)

# method 3
fit_fac3 <- aov(value ~ mtype + temp + mtype*temp, data = dat_5_3)
anova(fit_fac3)


# interaction plot
interaction.plot(dat_5_3$temp, dat_5_3$mtype, response = dat_5_3$value,
                 ylim = c(0,175), xaxt = "n",
                 xlab = "Temperature",
                 ylab = "Average life",
                 main = "Interaction between Material type and temperature",
                 legend = F, col = c("blue","red","green"))
axis(1, at=c(1,2,3), labels = c("15","70","125"), lwd.ticks = 3)
legend("topright", paste0("Type", 1:3), lty = 3:1, 
       col = c("blue","red","green"))

#text(locator(1), "Type 1", col = "blue")   #그래프에 텍스트 추가를 원할떄 사용
#text(locator(1), "Type 2", col = "red")
#text(locator(2), "Type 3", col = "green")

# The Assumption of No interaction
fit_noint <- aov(value ~ mtype + temp, data = dat_5_3)
anova(fit_noint)

#interaction effect term이 존재.
anova(fit_fac)
