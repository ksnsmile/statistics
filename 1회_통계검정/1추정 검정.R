######################
#모평균의 추정 및 검정

x <- rnorm(25, 70, 8) #25개의 난수 생성 , 정규분포 70평균 8표준편차

mean(x)
var(x)
sd(x)

t.test(x, mu=75)
t.test(x, mu=75, alternative = c("two.sided")) #양측 검정 평균이 같은지 다른지
t.test(x, mu=75, conf.level = 0.99)     # t.test(x, mu=75, alternative = c("greater")) 무의미하여 진행하지 않음 !!
t.test(x, mu=75, alternative = c("less"))


######################
#모분산의 추정 및 검정
install.packages("TeachingDemos")
library(TeachingDemos)
x <- rnorm(20, mean = 15, sd = 7)
x

mean(x)
var(x)

sigma.test(x, sigma = 6)
sigma.test(x, sigmasq = 36)
sigma.test(x, sigma = 6, alternative = "greater")


#############################
#모평균의 차이의 추정 및 검정
X=c(1:10)
X

mean(x)
var(x)

y=c(7:20)
y

mean(y)
var(y)

t.test(x, y )  #분산이 같지 않다고 가정하고 함
t.test(x,y, var.equal=TRUE) #분산이 같다고 가정하고 함


##################################
#모분산의 차이에 대한 추정 및 검정
x <- rnorm(25, mean = 15, sd = 9)
x
y <- rnorm(20, mean = 10, sd = 7)
y

length(x);  mean(x);  var(x)
length(y);  mean(y);  var(y)


var.test(x, y)          # Do x and y have the same variance?
var.test(x, y, alternative = c("greater")) 
var.test(lm(x ~ 1), lm(y ~ 1)) # The same.