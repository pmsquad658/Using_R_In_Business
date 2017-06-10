#############################################################
###Section 6. 집객효과가 가장 큰 광고의 조합은 무엇인가? ####
#############################################################

setwd("C:/R/비즈니스활용사례로배우는데이터분석R/sample-data")

# csv 파일 로드

ad.data <- read.csv("ad_result.csv", header=T, stringsAsFactors = F)

## 산점도 그리기

library("ggplot2")
library("scales")

#TV 광고의 광고비용과 신규 유저수의 산점도 그리기
ggplot(ad.data, aes(x=tvcm, y=install)) + geom_point() +
  xlab("TV광고비") + ylab("신규유저수") +
  scale_x_continuous(label=comma) +
  scale_y_continuous(label=comma)

#잡지 광고의 광고비용과 신규 유저수의 산점도 그리기
ggplot(ad.data, aes(x=magazine, y=install)) + geom_point() +
  xlab("잡지 광고비") + ylab("신규 유저수") +
  scale_x_continuous(label= comma) +
  scale_y_continuous(label= comma)

## 회귀분석 수행
fit <- lm(install ~., data=ad.data[,c("install","tvcm","magazine")])
# fit <- lm(install = tvcm + magazine, data=ad.data[,c("install","tvcm","magazine")])
fit

# summary 확인
summary(fit)

# 결과해석
# Residuals : 잔차(예측값과 측정값의 차이), 사분위수로 표현. 데이터의 치우침이 있는지 확인
# 여기서는 1Q 의 절댓값이 3Q 의 절댓값보다 커서 분포에 약간의 치우침이 있어 보이나
# 자유도 조정 결정계수가 0.92 로 여전히 높기 때문에, 광고 전략을 위한 의사결정에는 문제가 없어 보임



######################################################################
##### MeMo ###########################################################

lm(A~. , data=ZZ[, c("A", "B", "C")])
# A = B+C 모델을 의미. '~' 는 수식에서 '='에 해당. '.'는 data에 지정한 데이터에
# A 를 제외한 모든 항목을 + 로 이어달라는 뜻의 생략기호.

