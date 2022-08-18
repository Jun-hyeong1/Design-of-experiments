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

#기말고사에서 library만 하기. 

#기말고사에는 코드로 컴파운딩 하기까지가 7장의 문제이다. 

# 4개 블락에 콘파운딩하기 

A=B=C=D=E=c(-1,1) 



dat7_3 = expand.grid(A=A,B=B,C=C,D=D,E=E)
dim(dat7_3)
rname2 = make_rname(dat7_3,5, colnames(dat7_3))
rownames(dat7_3)<-(rname2)
dat7_3


# L1:x1+x4+x5   
# L2:x2+x3+x5

L1=c(1,0,0,1,1)
L2=c(0,1,1,0,1)


#-1을 0으로 변환
dat7_3[dat7_3==-1]=0
dat7_3

#L1을 이용해서 나머지로 구분.
L1_idx= apply(dat7_3, 1, function(x) (sum(x*L1))%%2)   #apply, 각 행에 대해 함수적용
L2_idx= apply(dat7_3, 1, function(x) (sum(x*L2))%%2)   #1은 행단위를 의미.  #%%는 나머지 

#블락을 만들자

blk1= dat7_3[(!as.logical(L1_idx))&(!as.logical(L2_idx)),]   #행에 대한 인덱싱 
blk2= dat7_3[(as.logical(L1_idx))&(!as.logical(L2_idx)),]    #&은 논리곱 
blk3= dat7_3[(!as.logical(L1_idx))&(as.logical(L2_idx)),]   
blk4= dat7_3[(as.logical(L1_idx))&(as.logical(L2_idx)),]

#블락별로 나눴다. 

blk1
blk2
blk3
blk4


####################################################################################
####################################################################################
#8.1 FFD, I=ABCD 일때. ##############################################################
####################################################################################


A=B=C=c(-1,1) 



dat8_1 = expand.grid(A=A,B=B,C=C)
dim(dat8_1)
dat8_1$D = apply(dat8_1,1,function(x) prod(x))    # 각 행을 다 곱해서 D열 만들기
dat8_1


#열이름을 만들어주자


rownames(dat8_1)= make_rname(dat8_1,4, colnames(dat8_1))
dat8_1

dat8_1$y<- c(45,100,45,65,
            75,60,80,96);dat8_1


#콘트라스트를 만들어주자

cont=model.matrix(~-1+A*B*C*D,data= dat8_1)   
head(cont)

#alias를 찾자.
ali_str = alias(lm(y~A*B*C*D, data= dat8_1))
ali_str$Complete                              #alias가 된 성분이 1 


eff_vec = (dat8_1$y %*% cont[,1:7])/4         #effect 추정량(벡터)
eff_vec 


ss_vec = (dat8_1$y %*% cont[,1:7])^2/8        #제곱합 추정량(벡터)
ss_vec 


fit = aov(y~A+B+C+D+A:B+A:C+B:C, data=dat8_1)
summary(fit)                                  #제곱합 추정량과 같은값
 





####################################################################################
####################################################################################
#8.2 FFD, I=ABCDE 일때. 
####################################################################################
#위와 거의 같다. 
####################################################################################



A=B=C=D=c(-1,1) 



dat8_2 = expand.grid(A=A,B=B,C=C,D=D)
dim(dat8_2)
dat8_2$E = apply(dat8_2,1,function(x) prod(x))    # 각 행을 다 곱해서 D열 만들기
dat8_2


#열이름을 만들어주자


rownames(dat8_2)= make_rname(dat8_2,5, colnames(dat8_2))
dat8_2

dat8_2$y<- c(8,9,34,52,
             16,22,45,60,
             6,10,30,50,
             15,21,44,63);dat8_2


#콘트라스트를 만들어주자

cont=model.matrix(~-1+A*B*C*D*E,data= dat8_2)   
head(cont)

#alias를 찾자.      #그냥 코드 외워자 이건 
ali_str = alias(lm(y~A*B*C*D*E, data= dat8_2))
ali_str$Complete                              #alias가 된 성분이 1 

colnames(ali_str$Complete[,-1])
n_eff = ncol(ali_str$Complete)-1
n_eff                                         #추정할  eff의 개수 

colnames(cont[,1:n_eff])                      #1:15번째 열이 우리의 추정할 eff


eff_vec = (dat8_2$y %*% cont[,1:15])/8         #effect 추정량(벡터)
eff_vec 


ss_vec = (dat8_2$y %*% cont[,1:15])^2/16        #제곱합 추정량(벡터)
ss_vec 


#어떤 effect가 유의한지 qq플롯을 그려보자 
qvalue= qqnorm(eff_vec, ylim=c(-5,40))

qqline(eff_vec)

text(x=qvalue$x, y=eff_vec+1,
     labels=colnames(eff_vec))


#위에서 판단한 효과로 모델링
fit= aov(y~A+B+C+A:B, data = dat8_2)
summary(fit)


#잔차 검정
par(mfrow=c(2,2))
plot(fit)









