rm(list=ls())

# 12주차 Chapter7
# block과 다른 요소들은 교호작용 없음

# example 7.1  #이건 그냥 블락킹, 콘파운딩 아니잖아. 
A=c(-1,1)
B=c(-1,1)
blk=factor(1:3)

dat7_1 <- expand.grid(A=A, B=B, blk=blk)
dat7_1$y <- c(28,36,18,31,
              25,32,19,30,
              27,32,23,29)

blk_total <- tapply(dat7_1$y, dat7_1$blk, sum)    #각 블락별 합 
blk_total

ss_blk <-sum(blk_total^2/4) - sum(dat7_1$y)^2/12 ; ss_blk   #SS_block


summary(aov(y ~ A*B + blk, data = dat7_1))        #ANOVA 


#만약 블락을 하지 않으면 

summary(aov(y ~ A*B , data = dat7_1))


####################################################################################

#2^3  디자인, 

# confounding the 2^k Factorial Design in the 2 blocks(p=1)

A<- B <- C<- c(-1,1)                                             #이런 것도 된다. 
dat <- expand.grid(A=A, B=B, C=C)                  

nfactor = 3                                       #A,B,C 는 세개
x=dat                                  #방금 만든 데이터프레임
cname = colnames(dat)                  #위 데이터 프레임이 이름 열


make_rname = function(x, nfactor, cname){                        #effect에 맞는 열이름 만드는 함수 
  subdat = x[,1:nfactor]
  rname = c()
  
  for(i in 1:nrow(x)){
    bool <- subdat[i,] == 1
    if(sum(bool)==0) rname[i] = "(1)"                             #합이 0이면 (1)을 넣어라. 
    else rname[i] = paste(tolower(cname[bool]), collapse = "")
  }
  return(rname)
}

rname <- make_rname(dat, 3, colnames(dat))       #dat의 
rownames(dat) <- rname 
 

rname2<-c("(1)", "a",   "b" ,  "ab" , "c"   ,"ac" , "bc",  "abc")   #어려우면 그냥 이렇게 넣어라. 

rownames(dat) <- rname2

dat
#-------------------------------------------------------------------------------------------------

tmp_dat <- dat
tmp_dat[tmp_dat==1] =0

blk_idx <- apply(tmp_dat, 1, function(x) sum(x) %%2)

blk1 = dat[!as.logical(blk_idx),] ; blk1
blk2 = dat[as.logical(blk_idx),] ; blk2

rownames(dat) <- c("(1)","a","b","ab","c","ac","bc","abc")                           #아래에서 응용한다. 



# ch7 part2######################################################################################

make_rname


# example 7.2
A<-B<-C<-D<-c(-1,1)
dat7_2 <- expand.grid(A=A, B=B, C=C, D=D)
dat7_2$y <-c(25,71,48,45,
             68,40,60,65,
             43,80,25,104,
             55,86,70,76)

rname <- make_rname(dat7_2, 4, colnames(dat7_2)[1:4])
rownames(dat7_2) <- rname
dat7_2


#블락 구분하는 법 

tmp_dat7_2 <- dat7_2[,1:4]                     #관측치 y열 제거 
tmp_dat7_2[tmp_dat7_2 == -1]=0                         #-1을 0으로 바꿔줘라 
tmp_dat7_2
blk_idx2 <- apply(tmp_dat7_2,1,function(x) sum(x)%%2)  # 1과 0으로 블락을 지시하는 벡터 (나머지 구하기!)

blk1 <- dat7_2[!as.logical(blk_idx2),] ; blk1            
blk2 <- dat7_2[as.logical(blk_idx2),] ; blk2  #위에서 만든벡터를 기준으로 블락에 구분하기(0이면 블락1로)







cont <- model.matrix(~ -1 + A*B*C*D, data = dat7_2)   #cont 매트릭스 




effect <- dat7_2$y %*% cont / 2^3 ; effect            #effect 추정
ss_vec <- (dat7_2$y %*% cont)^2 / 2^4 ; ss_vec          #제곱합 추정 

blk_eff <- mean(blk1$y) - mean(blk2$y) ; blk_eff      #블락 제곱합 

dat7_2$blk <- dat7_2$A * dat7_2$B * dat7_2$C *dat7_2$D  #blk라는 열을 생성한다. 


anova(aov(y ~ blk + A + C + D + A:C +A:D, data = dat7_2))   #ANOVA





#############################################################################################여기까지 12주차차

## confounding the 2^k FD in the 4 blocks
E <-c(-1,1)

dat7_3<- expand.grid(A=A, B=B, C=C, D=D, E=E)

rname2<- make_rname(dat7_3, 5, colnames(dat7_3))
rownames(dat7_3) <- rname2


L1 <- c(1,0,0,1,1) # confounding ADE
L2 <- c(0,1,1,0,1) # confounding BCE

dat7_3[dat7_3== -1] <- 0 
L1_idx = apply(dat7_3, 1, function(x) (sum(x * L1)) %%2)
L2_idx = apply(dat7_3, 1, function(x) (sum(x * L2)) %%2)

blk1 <- dat7_3[(!as.logical(L1_idx)) & (!as.logical(L2_idx)),] ; blk1
blk2 <- dat7_3[(as.logical(L1_idx)) & (!as.logical(L2_idx)),] ; blk2
blk3 <- dat7_3[(!as.logical(L1_idx)) & (as.logical(L2_idx)),] ; blk3
blk4 <- dat7_3[(as.logical(L1_idx)) & (as.logical(L2_idx)),] ; blk4

##########################################################################
A=B=C=D= factor(c(-1,1))
dat <- expand.grid(A=A, B=B, C=C, D=D)
dat$value <- c(45,71,48,65,68,60,90,65,43,100,45,104,75,86,70,96)

fit<- aov(value ~ A + A:B + C + A:B:C, dat=dat)
anova(fit)
# B에 대한 자유도가 AB에 합쳐져서 이상하게 나오는 것(자유도가 3)
# BC, AC들이 모델에 포함이 안되어서 그런 것 
