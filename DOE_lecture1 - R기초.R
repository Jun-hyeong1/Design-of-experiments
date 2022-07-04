# -- set working directoty ---
setwd("")
getwd()


# --- Data load ---
exam1 <- read.table("example_1.txt", sep = '\t', header = T)
exam1


# --- Data save ---
write.table(exam1,file="cars.txt", sep="\t", row.names = F)
write.csv(exam2,file="iris.csv",row.names = T)


# --- Object ---
# 1) Vector object
x <- c(1,2,3,4,5,6,7); x
typeof(x)

x[3]
x[3:5]



# 6번째 위치의 값을 9로 변환.
x[6] <- 9; x

# 8번째 위치에 10을 추가.
x[8] <- 10; x

# 2) Matrix object
test_matrix <- matrix(1:9, nrow=3, byrow=T, 
                      dimnames = list(c("a","b","c"),c("d","e","f"))); test_matrix

test_matrix[1,]
test_matrix[,2]


# 3) Factor object
x_fac <- factor(c("1","2","3","2","1","1"), levels= 1:5)
# level : 값들의 라벨, ordered=True : 순서형, ordered =False : 명목형, default : False
x_fac
x_fac[3]
x_fac[3:5]
# 7번째 위치에 5추가.
x_fac[7] <- "5"
x_fac

# 4) Dataframe Object
dat2 <- data.frame(num = c(rep(0,3),seq(1,2)), 
                   alpha = c("a","b","c","d","e"), stringsAsFactors = F)
# stringsAsFactors = F : data.frame()을 생성할 때, 변수에 문자가 있는 경우 자동으로 factor타입으로 변환 된다.
# 하지만 factor 변수는 연산되지 않으므로 stringsAsFactors() 함수를 써서 factor 타입으로 변환되지 않게 한다.
dat2  
dat2[3,1]
dat2[,1]  
dat2[3,]

dat2$num
dat2$alpha  

# 5) List object
test_list <- list(x = x, dat2 = dat2, mat = test_matrix); test_list

test_list[[1]]
test_list[[1]][3]
test_list$x[3]

test_list2 <- list(x_fac = x_fac, test_list = test_list); test_list2

test_list2[[2]]
test_list2[[2]][[2]]
test_list2[[2]][[2]][,1]


# --- type 확인 ---
x <- 5
is.numeric(x)
is.character(x)


# --- type 변환 ---
as.character(123)
as.numeric(c(T,F,T,T,F))
as.factor(c("apple","banana"))


# --- 사칙연산 ---
x <- c(1,2,3,4); x



x^3
x %/% 3   #몫
x %% 3    #나머지 

y <- c(1,3)
z <- c(1,4,8)



# --- 비교연산자 ---



# --- 논리연산자 ---
# 1) And 연산자
x <- c(T,T,F)
y <- c(F,T,F)

x & y
x && y    #맨 앞 하나만 비교

# 2) Or 연산자
x | y
x || y


# --- 조건문 ---
# 1) if statement


if((5>2) & (0>1)) {print("Out")}
if((5>2) | (0>1)) {print("Out")}

if(all(c(T,F))) {print("Logic is True")}
if(any(c(F,T))) {print("Logic is True")}

# 2) if-else statement 
if(F) {
  print("Logic is True")
} else {
  print("Logic is False")
}

score <- 95

if(score >= 90){
  print("A")
} else if(score >= 80) { 
  print("B")
} else {  
  print("C") 
}

# 3) ifelse statement
ifelse(c(1,2,3) > c(1,1,1), "Logic is True", "Logic is false")

score <- c(85,95,75)
ifelse(score >= 90,"A",ifelse(score>=80,"B","C"))

# --- 반복문 ---
# 1) for statement
for(i in 1:10){
  print(i)
}



sum_even = 0
for(i in 1:50){
  if(i%%2==0)
  {
    sum_even = sum_even + i
  }
}
sum_even



# 2) while statement

sum2 = 0
i = 1
while(i <= 100){
  sum2 = sum2 + i
  i = i + 1
}
sum2



