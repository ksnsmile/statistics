#6.6. 범주형 자료분석
#6.6.1.적합도 검정


#6.6.1.1. Chi-square Test

x <- c(A = 20, B = 15, C = 25)
x
Xsq <- chisq.test(x)
Xsq <- chisq.test(as.table(x))             # the same
Xsq
Xsq$observed   # 관찰 도수
Xsq$expected   # 귀무가설하에서의 기대 도수
Xsq$residuals  # Pearson's 잔차
sum(Xsq$residuals^2)   # 검정 통계량 값
Xsq$stdres     # 표준화 잔차

x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
chisq.test(x, p = p, rescale.p = TRUE)

x <- c(89,37,30,28,2)
p1 <- c(0.40,0.20,0.20,0.19,0.01)
chisq.test(x, p = p1) 


x <- trunc(10 * runif(1000))
table(x)
chisq.test(table(x))            # NOT 'chisq.test(x)'!


#6.6.1.2. Kolmogorov?Smirnov test

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)

x <- rnorm(50, 1, 2)
ks.test(x, "pt", 1) # two-sided, exact
ks.test(x, "pt", 1, exact = FALSE)
ks.test(x, "pt", 1, alternative = "gr")
ks.test(x, "pt", 1, alternative = "l")


# test if x is stochastically larger than x2
require(graphics)
x <- rnorm(50)
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim = range(c(x, x2)))
plot(ecdf(x2), add = TRUE, lty = "dashed")
t.test(x, x2, alternative = "g")
wilcox.test(x, x2, alternative = "g")
ks.test(x, x2, alternative = "l")


#6.6.1.3. Shapiro-Wilk Normality Test

shapiro.test(rnorm(25, mean = 5, sd = 3))
shapiro.test(runif(25, min = 2, max = 5))



#6.6.2. 독립성 검정
#6.6.2.1. Chi-square Test

library(MASS)   
TableSE = table(survey$Smoke, survey$Exer) 
TableSE
chisq.test(TableSE)  

TableSE1 = cbind(TableSE[,"Freq"], TableSE[,"None"] + TableSE[,"Some"]) 
TableSE1
chisq.test(TableSE1)    


#6.6.2.2. Fisher Exact Test

## Agresti (1990, p. 61f; 2002, p. 91) Fisher's Tea Drinker
## A British woman claimed to be able to distinguish whether milk or
##  tea was added to the cup first.  To test, she was given 8 cups of
##  tea, in four of which milk was added first.  The null hypothesis
##  is that there is no association between the true order of pouring
##  and the woman's guess, the alternative that there is a positive
##  association (that the odds ratio is greater than 1).
TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2,
                dimnames = list(Guess = c("Milk", "Tea"),
                                Truth = c("Milk", "Tea")))
TeaTasting
fisher.test(TeaTasting, alternative = "greater")
## => p = 0.2429, association could not be established



#6.6.3. 동일성 검정

#6.6.3.1 chi square test

Dices = matrix( c(38, 12, 26, 4, 26, 17, 34, 17, 31, 18, 45, 32), nrow=2,
          dimnames = list ( "시행" = c("시행 1", "시행 2"),     
                            "결과" = c("1", "2", "3", "4", "5", "6")))
Dices
chisq.test(Dices)


#6.6.3.2. McNemar Test

## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <- matrix(c(794, 86, 150, 570),
               nrow = 2,
               dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                               "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings
mcnemar.test(Performance, correct=FALSE)

#### McNemar과 비교
chisq.test(Performance)  
binom.test(86, 86+150, 0.5)




