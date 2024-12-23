#상관분석
#상관계수
## Correlation Matrix of Multivariate sample:

options(width=100)

longley
str(longley)

require(stats); require(graphics)
# library(stats); library(graphics)
# library 와 require 의 차이 ;  

pairs(longley, main = "자료명 : longley")

Cl <- cor(longley)
Cl

symnum(Cl) # highly correlated
symnum(clS <- cor(longley, method = "spearman"))
symnum(clK <- cor(longley, method = "kendall"))

i <- lower.tri(Cl)
cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))

# 패키지 "corrgram"설치
install.packages("corrgram")

library(corrgram)
corrgram(cor(longley), main = "상관계수의 시각화", type="corr",cor.method = "pearson", upper.panel=panel.conf)


#--- 결측치가 있는 경우
cov( cbind ( c(1,2,3,4), c(2,3,6,8), c(3,5,8,10) ))

cov( cbind ( c(1,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="everything")

cov( cbind ( c(1,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="all") #결측치가 있어도 그냥 계산


cov( cbind ( c(NA,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="everything")

#cov( cbind ( c(NA,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="all")

cov( cbind ( c(NA,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="complete")

cov( cbind ( c(2,3,4), c(3,6,8), c(5,8,10)), use="everything")

cov( cbind ( c(2,3,4), c(3,6,8), c(5,8,10)), use="all")

cov( cbind ( c(NA,2,3,4), c(2,3,6,8), c(3,5,8,10)), use="pairwise")

# "complete": 모든 변수에서 NA가 없는 행만 사용하여 계산.
# "everything": NA가 있는 경우 결과는 NA가 되며, NA가 없을 때는 모든 데이터를 사용.
# "all": NA가 있는 경우에도 가능한 모든 데이터를 사용.
# "pairwise": 각 변수 쌍에서 NA가 없는 데이터만 사용하여 개별적으로 계산.

#상관분석
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
plot(x,y)

## Compare this to
cor.test(x, y, method = "pearson", alternative = "g")
cor.test(x, y,                    alternative = "g")
cor.test(~ x+y,                   alternative = "g")

cor.test(x, y, method = "kendall", alternative = "greater")
cor.test(x, y, method = "kendall", alternative = "greater", exact = FALSE)

cor.test(x, y, method = "spearman", alternative = "greater")
cor.test(x, y, method = "spearman", alternative = "greater",exact = FALSE)