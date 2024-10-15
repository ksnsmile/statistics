#함수 “c()”를 이용한 자료의 입력
Sals= c(12, .4, 5, 2, 50, 8, 3, 1, 4, 0.25 )
Sals
max(Sals)
min(Sals)
mean(Sals)
var(Sals) 	
sd(Sals) 
median(Sals) 	
fivenum(Sals)
summary(Sals)


# DEMO 시연
demo(graphics)


# options에 대하여
help(options)                    # 이용 가능한 옵션
options()                        # 현재 옵션 설정 보기
options(digits=7)                # 3자리수로 표현하도록 설정(기본은 7자리로 되어 있음)