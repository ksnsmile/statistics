#6.5. 비모수통계학
#6.5.1. 부호 검정
binom.test
## Conover (1971), p. 97f.
## Under (the assumption of) simple Mendelian inheritance, a cross
##  between plants of two particular genotypes produces progeny 1/4 of
##  which are "dwarf" and 3/4 of which are "giant", respectively.
##  In an experiment to determine if this assumption is reasonable, a
##  cross results in progeny having 243 dwarf and 682 giant plants.
##  If "giant" is taken as success, the null hypothesis is that p =
##  3/4 and the alternative that p != 3/4.
binom.test(c(682, 243), p = 3/4)
binom.test(682, 682 + 243, p = 3/4)   # The same.
## => Data are in agreement with the null hypothesis.


# [Ex 12.1]
library(DescTools)
x <- c(96, 110,  98, 113, 88,  92, 106, 119, 100, 97)
y <- c(99, 112, 107, 110, 88, 101, 107, 123,  91, 99)
SignTest(x,y,alternative="less")


#6.5.2. Wilcoxon Rank Sum and Signed Rank Tests
#6.5.2.1. Wilcoxon Signed Rank Test

## One-sample test.
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).

x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")   
wilcox.test(y - x, alternative = "less") # The same.
wilcox.test(y - x, alternative = "less", exact = FALSE, correct = FALSE)

# [Ex 12.3]
x <- c(70, 85, 73, 75, 65, 50, 80, 71, 80, 51)
y <- c(65, 41, 45, 80, 84, 50, 71, 52, 42, 78)
wilcox.test(x, y, paired=TRUE)
wilcox.test(x, y, paired=TRUE, exact = FALSE, correct = FALSE)


# 6.5.2.2. Wilcoxon Rank Sum Test
#( two sample test, Mann-Whitney’ test, U-test)

# [Ex 12.5]
A <- c(4.7, 6.4, 4.1, 3.7, 3.9)
B <- c(7.6, 11.1, 6.8, 9.8, 4.9, 6.1, 15.1)
SumRankX = sum(rank(c(A,B))[1:5]) 
SumRankX
SumRankX-(length(A)*(length(A)+1))/2
wilcox.test(A, B, alternative = "less")


# [Ex 12.6]
A <- c(253, 165, 197, 216, 201, 200, 202, 228, 175, 182, 199)
B <- c(155, 162, 148, 160, 173, 138, 143, 174, 164, 148, 153, 190, 213, 142, 141)

SumRankX = sum(rank(c(A,B))[1:11]) 
SumRankX
(length(A)*(length(A)+1))/2
SumRankX-(length(A)*(length(A)+1))/2

wilcox.test(A, B)
wilcox.test(A, B, correct=FALSE)
wilcox.test(A, B, exact=FALSE, correct=FALSE )


# Another Examples
wilcox.test(rnorm(10), rnorm(10, 2), conf.int = TRUE)


X1 <- round(rnorm(5,100,10),digits = 0)
X1
X2 <- round(rnorm(6,110,10), digits = 0)
X2
wilcox.test(X1, X2, conf.int = TRUE)


## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
boxplot(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
wilcox.test(Ozone ~ Month, data = airquality, exact=FALSE,
            subset = Month %in% c(5, 8))



# 6.5.3. Kruskal-Wallis Rank Sum Test

## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
kruskal.test(list(x, y, z))


## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))
kruskal.test(x, g)


## Formula interface.
require(graphics)
boxplot(Ozone ~ Month, horizontal = TRUE, data = airquality)
kruskal.test(Ozone ~ Month, data = airquality)




# 6.5.4. Run Test
library(tseries)

# [Ex12.7]
X <- factor(c(1,1,1,-1,1,1,1,1,-1,-1,-1,1))
runs.test(X)

# [Ex12.8]
Y <- factor(c("D","P","P","P","P","D","P","P","P","P",
              "D","P","P","P","P","D","P","P","P","P",
              "D","P","P","P","P","D","P","P","P","P"))
runs.test(Y)


# Another Examples
X1 <- factor(sign(rnorm(100)))  # randomness
X1
runs.test(X1)

X2 <- factor(rep(c(-1,1),50))  # over-mixing
X2
runs.test(X2)