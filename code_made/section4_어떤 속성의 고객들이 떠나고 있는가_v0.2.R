######################################################
###Section 4. 어떤 속성의 고객들이 떠나고 있는가? ####
######################################################

setwd("C:/R/예제소스_데이터분석R_사카마키류지/DataAnalysis_src/R")

##  4장 소스코드

# CSV 파일을 읽어들이기
dau <- read.csv("section4-dau.csv", header = T, stringsAsFactors = F)
head(dau)
user.info <- read.csv("section4-user_info.csv", header = T, stringsAsFactors = F)
head(user.info)

# DAU에 user.info를 결합시키기
dau.user.info <- merge(dau, user.info, by = c("user_id", "app_name"))
dau.user.info <- dau.user.info[order(dau.user.info$user_id, dau.user.info$app_name, dau.user.info$log_date),]  # 오더링추가
rownames(dau.user.info) <- NULL
head(dau.user.info, 10)



# 월 항목을 추가
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7)

# 세그먼트 분석（성별로 집계）
table(dau.user.info[, c("log_month", "gender")])

# 세그먼트 분석(연령대별로 집계）
table(dau.user.info[, c("log_month", "generation")])

# 세그먼트 분석（성별과 연령대를 조합해 집계）
library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id",
      length)


# 세그먼트 분석（단말기별로 집계）
table(dau.user.info[,c("log_month","device_type")])

# 세그먼트 분석 결과를 시각화하기
library(plyr)

# 날짜별로 단말기별 유저수를 산출하기
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
# 날짜별 데이터 형식으로 변환하기
dau.user.info.device.summary$log_date <- as.Date(dau.user.info.device.summary$log_date)

  # format 형태를 변경하고 싶다면?
  # dau.user.info.device.summary$log_date <- format(dau.user.info.device.summary$log_date, format="%m/%d/%Y")

str(dau.user.info.device.summary)
# 시계열의 트렌드 그래프 그리기
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)


###############################################################################################
############    MeMo ##########################################################################

## IF ELSE 문 사용

ifelse (AA == BB, "XX", "YY")
AA$BB <- ifelse(AA$CC == AA$DD , "install" , "existing")



# ggplot 그래프 그릴 때 scale 에 대한 limits 값을 먼저 설정해 주고 scales 옵션에 넣어주는 것이 좋다
# scales 값에서 comma 설정해 주

library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)