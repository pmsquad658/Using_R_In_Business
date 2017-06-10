#########################################################
###Section 5. 어느 쪽의 배너광고가 반응이 더 좋은가? ####
#########################################################

setwd("C:/R/비즈니스활용사례로배우는데이터분석R/sample-data")

# ab.test.imp 데이터에 ab.test.goal 데이터를 결합시키기
# 데이터 로드

ab.test.imp <- read.csv("section5-ab_test_imp.csv", header=T, stringsAsFactors=F)
ab.test.goal <- read.csv("section5-ab_test_goal.csv", header=T, stringsAsFactors = F)

# 날짜변수 변환
ab.test.imp$log_date <- as.Date(ab.test.imp$log_date, format="%Y-%m-%d")
ab.test.goal$log_date <- as.Date(ab.test.goal$log_date, format="%Y-%m-%d")

#
# ab.test.imp 에 ab.test.goal 결합 시키기 
# suffixes 옵션 사용 : SQL Join 의 AS 설정과 같음
ab.test.imp <- merge(ab.test.imp, ab.test.goal, by="transaction_id", all.x=T, suffixes=c("",".g"))
head(ab.test.imp)

#
# 클릭 플래그 추가
ab.test.imp$is.goal <- ifelse(is.na(ab.test.imp$user_id.g),0,1)
table(ab.test.imp)

#
# 클릭률 계산하기
library(plyr)
ddply(ab.test.imp, .(test_case), summarize, cvr=sum(is.goal)/length(user_id))

#
# χ2 검정을 실행하기
chisq.test(ab.test.imp$test_case, ab.test.imp$is.goal)

#
# 날짜별, 테스트 케이스별로 클릭율을 산출하기
ab.test.imp.summary <-
  ddply(ab.test.imp, .(log_date, test_case), summarize,
        imp=length(user_id),
        cv=sum(is.goal),
        cvr=sum(is.goal)/length(user_id))

#
# 테스트 케이스별로 클릭률 산출하기
ab.test.imp.summary <-
  ddply(ab.test.imp.summary, .(test_case), transform,
        cvr.avg=sum(cv)/sum(imp))

#
# 테스트 케이스별 클릭율의 시계열추이 그래프

library(ggplot2)
library(scales)

limits <- c(0, max(ab.test.imp.summary$cvr))
ggplot(ab.test.imp.summary, aes(x=log_date, y=cvr, col=test_case, lty=test_case, shape=test_case))+
  geom_line(lwd=1)+
  geom_point(size=4)+
  # ggplot 에서 지정한 직선 외 또 그릴 수도 있다. 여기서는 y 값만 지정한 클릭률 전체평ㄱ
  geom_line(aes(y=cvr.avg, col=test_case))+
  scale_y_continuous(label=percent, limits=limits)균


######################################################
############  MeMo ###################################

# suffixes 옵션 사용 : SQL Join 의 AS 설정과 같음
merge(AA, BB, by="transaction_id", all.x=T, suffixes=c("",".b"))

# ddply 사용하여 집계하기
# user_id 를 freq 변수로 두고, is.goal 을 0,1 로 두어 sum 했을 때 1만 집계되도록 함
ddply(AA, .(test_case), summarize, cvr=sum(is.goal)/length(user_id))


