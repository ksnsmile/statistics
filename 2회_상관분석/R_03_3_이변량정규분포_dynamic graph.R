## 좌표의 범위 설정 
X <- seq(-3, 3, .01) -> Y # -3부터 3까지 0.1의 간격을 가지고 설정

install.packages("mvtnorm")
library(mvtnorm)

## x 와 y 의 평면 좌표값 설정
XY <- expand.grid(x=X, y=Y)


## 평균,  표준편차 및 상관계수 설정
Mean = c(0,0); Rho = 0.7; Sigma_1 = 1; Sigma_2 = 1;
Sigma =matrix(c(Sigma_1,Rho,Rho,Sigma_2),2,2);

##이변량정규분포 함수값 결정
Z <- dmvnorm(XY, mean=Mean, sigma=Sigma)

install.packages("rgl")
library(rgl)

persp3d(X,Y,matrix(Z,length(X),length(Y)),smooth=T,color=rgb(0.7,0.5,0.2, 0.5),
        axes=T,xlab="X",ylab="Y",zlab="BiNorm")
play3d(spin3d(axis=c(0,0,1),rpm=0.5), duration=100)

#plot3d(cbind(XY, z=Z),axes=T, xlab="X",ylab="Y",zlab="BiNorm")
#play3d(spin3d(axis=c(0,0,1),rpm=3))



