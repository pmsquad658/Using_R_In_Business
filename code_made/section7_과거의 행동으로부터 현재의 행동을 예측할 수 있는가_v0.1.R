######################################################################
###Section 7. 과거의 행동으로부터 현재의 행동을 예측할 수 있는가? ####
######################################################################

setwd("C:/R/비즈니스활용사례로배우는데이터분석R/sample-data")

# csv 파일 로드

dau <- read.csv("section7-dau.csv", header=T, stringsAsFactors = F)
head(dau)

## 유저별로 ID 이전을 한 유저인지 아닌지 나타내는 데이터 정리
# MAU : 1월과 2월에 게임을 이용한 유저의 데이터가 저장 
mau <- unique (dau[,c("region_month", "device", "user_id")])

# FP MAU
fp.mau <- unique (dau[dau$device=="FP", c("region_month", "device", "user_id")])

# SP MAU
sp.mau <- unique (dau[dau$device=="SP", c("region_month", "device", "user_id")])

## 1월과 2월 데이터 나누기
fp.mau1 <- fp.mau[fp.mau$region_month== "2013-01",]
fp.mau2 <- fp.mau[fp.mau$region_month== "2013-02",]

sp.mau1 <- sp.mau[sp.mau$region_month== "2013-01",]
sp.mau2 <- sp.mau[sp.mau$region_month== "2013-02",]

# 1월에 피처폰을 이용했던 유저가 2월에도 이용했는가
# 접속 여부에 대한 flag 값을 찍어줌
mau$is_access <- 1

# 피처폰 1월 이용자 중 id 기준으로 2월 접속 여부를 붙임 : 2월에도 계속 이용하는가? 
fp.mau1 <- merge(fp.mau1, mau[mau$region_month == "2013-02", c("user_id", "is_access")],
                 by="user_id", all.x=T)
# na 값 처리리
# 1, 0 으로 2월 접속 여부를 표기 완료 
fp.mau1$is_access[is.na(fp.mau1$is_access)] <- 0
head(fp.mau1, 20)


## 1월에 피처폰으로 이용했고 2월에도 피처폰으로 이용한 유저 구별 
fp.mau2$is_fp <-1
fp.mau1 <- merge(fp.mau1, fp.mau2[, c("user_id", "is_fp")], 
                 by= "user_id", all.x = T)
fp.mau1$is_fp[is.na(fp.mau1$is_fp)] <- 0
head(fp.mau1, 20)


## 1월에 피처폰으로 이용했고 2월에는 스마트폰으로 이용한 유저 구별 
sp.mau2$is_sp <- 1
fp.mau1 <- merge(fp.mau1, sp.mau2[, c("user_id", "is_sp")],
                 by="user_id", all.x = T)
fp.mau1$is_sp[is.na(fp.mau1$is_sp)] <- 0
head(fp.mau1, 20)


## 1월에는 피처폰으로 이용했는데 2월에는 이용하지 않았거나 혹은 스마트폰으로 이용한 유저
# 1월에 피처폰 이용자중 2월에 sp 전환 비중을 보고자 함 

# 로지스틱 회귀분석에 필요한 데이터 : 앞의 데이터에서 is_access=1, is_sp=0 인 데이터가 제외됨
fp.mau_final <- fp.mau1[fp.mau1$is_access == 0 | fp.mau1$is_sp == 1, ]

## 날짜별 게임 이용 상황 데이터 정리하기
library("reshape2")

fp.dau1 <- dau[dau$device == "FP" & dau$region_month == "2013-01",]
fp.dau1$is_access <- 1


fp.dau1.cast <- dcast(fp.dau1, user_id ~ region_day, value.var =
                        "is_access", function(x) as.character(length(x)))
# dcast(원 데이터, 행이 될 항목 ~ 열이 될 항목, value.var = "값으로 사용할 항목", 결과 형태)
# 함수 안쓰면 디폴트로 length(x) 수행
# dcast(x, date ~ product, value.var = "sales", sum)

#test1 <- dcast(fp.dau1, user_id ~ region_day, value.var =
#                 "is_access", function(x) length(x))

names(fp.dau1.cast)[-1] <- paste0("X", 1:31, "day")

# 2월에 스마트폰으로 이용한 유저 데이터 결합하기
fp.dau1.cast <- merge(fp.dau1.cast, fp.mau_final[,c("user_id", "is_sp")], by="user_id")

table(fp.dau1.cast$is_sp)

## 로지스틱 회귀분석을 통한 모델 작성

fit.logit <- step(glm(is_sp ~ ., data=fp.dau1.cast[, -1], family = binomial))
summary(fit.logit)

# step : AIC (아카이케 정보 척도)를 사용하여 모델에 사용할 설명변수를 늘릴지 혹은
# 줄일지 자동으로 계산

## 작성된 모델을 이용해서 예측하기 

# sp(스마트폰) 이전 확률  
fp.dau1.cast$prob <- round(fitted(fit.logit), 2)

# sp(스마트폰)로 이전할지 예측
# 모델의 신뢰성 분석을 위한 pred 값 추가. 예측확률이 0.5 이상인 것에 flag 1 표시 
fp.dau1.cast$pred <- ifelse(fp.dau1.cast$prob > 0.5, 1, 0)
head(fp.dau1.cast)

# 예측과 실제
table(fp.dau1.cast[, c("is_sp", "pred")])


## 예측결과로부터 유저군 추측하기 
fp.dau1.cast1 <- fp.dau1.cast[fp.dau1.cast$is_sp == 1 & fp.dau1.cast$pred == 1,]
head(fp.dau1.cast1[order(fp.dau1.cast1$prob, decreasing = T),], 100)


# 예측은 1 인데 실제로는 0 인 유저 이용상황 체크  
fp.dau1.cast2 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred ==1,]
head(fp.dau1.cast2[order(fp.dau1.cast2$prob, decreasing = T),])


### 정리
# 1월에 피처폰을 쓴 유저중에, 2월에 access==0 인 유저 + 2월에 스마트폰을 이용한 유저 추출 : 탈퇴 또는 sp 전환
# 1월에 피처폰 이용자 데이터를 날짜별로 dcast 한 데이터 셋을 만들고 거기에 위 이용상황 컬럼 merge
# 1월에 피처폰을 이용한 유저중에 탈퇴한 유저는 is_sp == 0 으로, 스마트폰으로 옯겨 이용중은 is_sp == 1 로 구분됨 
# 로지스틱 회귀분석에서는 1월 피처폰 이용자 중에서 탈퇴 또는 스마트폰 전환에 대한 예측모델을 수행함 
# 여기서 알아보려 한 점은 2월에 sp 로 전환할 것으로 예측했는데 실제로는 탈퇴해 버린 10명에 대해


#######################################################################
############ MeMo #####################################################

# AA 라는 데이터에서 항목 XX 와 YY, ZZ 가 중복되지 않도록 정리 -> 모두 같은 값일 때
# 중복처리하여 하나만 남김
unique(AA[ , c("XX", "YY", "ZZ")])

# AA라는 데이터에서 AA의 XX항목이 xxx인 것만 추출하고 X,Y,Z 의 항목은 중복제거
unique(AA[AA$XX=="xxx", c("X", "Y", "Z")])

# dcast(원 데이터, 행이 될 항목 ~ 열이 될 항목, value.var = "값으로 사용할 항목", 결과 형태)
# 함수 안쓰면 디폴트로 length(x) 수행
# dcast(x, date ~ product, value.var = "sales", sum)
fp.dau1.cast <- dcast(fp.dau1, user_id ~ region_day, value.var =
                        "is_access", function(x) as.character(length(x)))


