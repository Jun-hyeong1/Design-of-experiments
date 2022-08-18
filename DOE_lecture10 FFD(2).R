rm(list=ls())


make_rname = function(x,nfactor,cname){
  
  subdat = x[,1:nfactor]
  rname = rep(0,nrow(x))
  i=2
  for(i in 1:nrow(x)){
    bool = subdat[i,]==1
    if(sum(bool)==0) rname[i]="(1)"
    else rname[i] = paste(tolower(cname[bool]),collapse="")
  }
  return(rname)
}



####################################################################################
####################################################################################
#8.4 FFD, 2^(6-2) 일때. 
####################################################################################
#앞장과 거의 같다. 
####################################################################################



A=B=C=D=c(-1,1) 



dat8_4 = expand.grid(A=A,B=B,C=C,D=D)


dat8_4$E = apply(dat8_4,1,function(x) prod(x[1:3]))   #E = ABC
dat8_4$F = apply(dat8_4,1,function(x) prod(x[2:4]))   #F = BCD
dat8_4


#열이름을 만들어주자


rownames(dat8_4)= make_rname(dat8_4, 6, colnames(dat8_4))
dat8_4
data4

#콘트라스트를 만들어주자

cont=model.matrix(~ -1 + (.)^6, data= dat8_4)   #왜-1인가?? 알아야지.
head(cont)


dat8_4$y<- c(6,10,32,60,
             4,15,26,60,
             8,12,34,60,
             16,5,37,52);dat8_4




#alias를 찾자.      #그냥 코드 외워자 이건 
ali_str = alias(lm(y~ (.)^6, data= dat8_4))
ali_str$Complete                              #alias가 된 성분이 1 




eff_var = colnames(ali_str$Complete)[-1]

vec_idx = which(colnames(cont) %in% eff_var)



eff_vec = (dat8_4$y %*% cont[,vec_idx])/8         #effect 추정량(벡터)
eff_vec 


ss_vec = (dat8_4$y %*% cont[,vec_idx])^2/16        #제곱합 추정량(벡터)
ss_vec 



#어떤 effect가 유의한지 qq플롯을 그려보자 
qvalue = qqnorm(eff_vec, ylim= c(-10,45))
qqline(eff_vec)

text(x=qvalue$x, y=eff_vec+1,              #잘보이려고 +1 
     labels=colnames(eff_vec))                          #A,B,A:B가 유의하다 




#위에서 판단한 효과로 모델링링
fit= aov(y~ A*B, data = dat8_4)
summary(fit)


#잔차 검정
par(mfrow=c(1,2))
qqnorm(fit$residuals)
qqline(fit$residuals)

plot(x=dat8_4$C, y=fit$residuals, 
     xlab = "C", ylab = "Residuals")

abline(h=0, col="red")




#dispersion Effects 를 계산하자  


new_count = cont[,vec_idx]

res_fit = residuals(fit)

# s_ip : 부호가+
# s_im : 부호가-

s_ip = s_im = rep(0, ncol(new_count))

length(s_ip)

new_count[,1]


i=1

for(i in 1:ncol(new_count)){
  s_ip[i] = sd(res_fit[new_count[,i]==1])
  s_im[i] = sd(res_fit[new_count[,i]==-1])
  
}

s_ip;s_im



f_istar = log(s_ip^2/s_im^2)
f_istar

#plot 을 그려보자
par(mfrow=c(1,1))
qvalue = qqnorm(f_istar, ylim = c(-1,3))
qqline(f_istar)

text(x=qvalue$x, y=f_istar+0.2,              #잘보이려고 +0.2
     labels=tolower(colnames(new_count))) 

#기하적으로 어떤 factor가 유의 할까

#y 반응 
tapply(dat8_4$y, list(A=dat8_4$A, B=dat8_4$B, C=dat8_4$C ), mean)

#range
tapply(dat8_4$y, list(A=dat8_4$A, B=dat8_4$B, C=dat8_4$C ), 
       function(x) max(x)-min(x))










