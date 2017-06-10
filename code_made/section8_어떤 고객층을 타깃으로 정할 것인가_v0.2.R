######################################################################
###Section 9. 어떤 고객층을 타깃으로 정할 것인가?                 ####
######################################################################

setwd("C:/R/비즈니스활용사례로배우는데이터분석R/sample-data")

## 데이터를 읽어들이기 위한 함수 작성
install.packages("foreach")
install.packages("ykmeans")
install.packages("ggplot2")
lnstall.packages("scales")

library("plyr")
library("foreach")
library("ykmeans")
library("ggplot2")
library("scales")

# 지정된 날짜의 데이터를 취득하는 함수를 작성 

readTsvDates <- function(base.dir, app.name, date.from, date.to) {
    date.from <- as.Date(date.from)
    date.to   <- as.Date(date.to)
    dates     <- seq.Date(date.from, date.to, by = "day")
    x <- ldply(foreach(day = dates, combine = rbind) %do% {
      read.csv(sprintf("%s/%s/%s/data.tsv", base.dir, app.name, day),
               header = T, sep = "\t", stringsAsFactors = F)
      
    })
    x
    
}


# DAU를 읽어 들이는 함수를 작성 

readDau <- function(app.name, date.from, date.to = date.from) {
    data <- readTsvDates("section8/daily/dau", app.name, 
                         date.from, date.to)
    
    # section8/daily/dau/app.name/dates 의 값을 읽어서 rbind 한 데이터프레임을 반환하여
    # data 에 저장함
    
    data
}

# DPU를 읽어 들이는 함수 작성 

readDpu <- function(app.name, date.from, date.to = date.from) {
    data <- readTsvDates("section8/daily/dpu", app.name,
                         date.from, date.to)
    data
}

# 행동 데이터를 읽어 들이는 함수 작성  
 
readActionDaily <- function(app.name, date.from, date.to = date.from) {
    data <- readTsvDates("section8/daily/action", app.name, date.from, date.to)
    
    data
}


## 데이터 읽어 들이기

# DAU
dau <- readDau("game-01", "2013-05-01", "2013-10-31")
head(dau)

# DPU
dpu <- readDpu("game-01", "2013-05-01", "2013-10-31")
head(dpu)

# Action
user.action <- readActionDaily("game-01", "2013-10-31", "2013-10-31")
head(user.action)

## DAU 에 DPU 결합시키기

# 과금 데이터 결합 
dau2 <- merge(dau, dpu[, c("log_date", "user_id", "payment"),], 
              by = c("log_date", "user_id"), all.x = T)



# 과금 플래그 추가하기 
dau2$is.payment <- ifelse(is.na(dau2$payment), 0, 1)
head(dau2)

# 비과금 레코드의 과금액에 0 넣기
dau2$payment <- ifelse(is.na(dau2$payment), 0, dau2$payment)
table(dau2$is.payment) # 과금액 유 확인

## 월별로 집계하기 

# 월 항목 작성하기
dau2$log_month <- substr(dau2$log_date, 1, 7)

# 월별로 집계하기
mau <- ddply(dau2, .(log_month, user_id), summarize, payment = sum(payment),
             access_days = length(log_date))

## 랭킹 범위 결정

# A47 항목이 랭킹 포인트 

user.action2 <- ykmeans(user.action, "A47", "A47", 3)

# 각 클러스터의 유저수

table(user.action2$cluster)

## 랭킹 포인트의 분포

#
ggplot(arrange(user.action2, desc(A47)), 
       aes(x=1:length(user_id), y= A47, 
           col= as.factor(cluster), shape= as.factor(cluster))) +
  geom_line() +
  xlab("user") +
  ylab("Ranking point") +
  scale_y_continuous(label=comma) +
  ggtitle("Ranking Point") +
  theme(legend.position = "none")

## 랭킹 상위 유저만 고르기 
user.action.h <- user.action2[user.action2$cluster >= 2, names(user.action)]


## 주성분 분석 실행 
# 데이터 전처리에 편리한 함수를 이용하기 위해 
# 기계학습 라이브러리를 사용함 
library("caret")

user.action.f <- user.action.h
head(user.action.f)

# 정보량이 0에 가까운 변수를 삭제
nzv <- nearZeroVar(user.action.f)
user.action.f.filterd <- user.action.f[,-nzv]

# 변수 간에 상관이 높은 것을 삭제
user.action.cor <- cor(user.action.f.filterd)
user.action.cor
highly.cor.f <- findCorrelation(user.action.cor, cutoff=.7)
highly.cor.f
user.action.f.filterd <- user.action.f.filterd[,-highly.cor.f]

# 주성분 분석 실행 
# pca

user.action.pca.base <- prcomp(user.action.f.filterd[,-1], scale = T, center=T)

names(user.action.pca.base)

# 고유값 확인 
user.action.pca.base$sdev^2
# -> 1~6 주성분까지 1이상으로 유효 

# 누적 변동 기여율 값 확인 : 80% 이상 유효 
summary(user.action.pca.base)

# 부하 : 선
user.action.pca.base$rotation

# biplot 으로 주성분 산점도 그리기 : 주성분 점수에 대한 산점도 
library(Hmisc)
biplot(user.action.pca.base, choices=1:2)
biplot(user.action.pca.base, choices=3:4)
biplot(user.action.pca.base, choices=c(1,3))


## 클러스터링 실행하기 

user.action.pca <- data.frame(user.action.pca.base$x) # x 는 주성분분석 후 rotation 된 선형결합 값 
keys <- names(user.action.pca) # PC1 ~ PC19

# 3~6 개 범위 내 클러스터링을 실시 : 클라이언트 요구 
# k-means 알고리즘으로 제1주성분의 확산 범위가 가장 작은 클러스터 수를 찾기
# 클러스터수가 5일 때 확산 범위가 가장 작게 나왔으므로 5개로 결정이 됨. table 상 cluster는 빈도값을 나타냄 
# 벡터값 (19개) 모든 클러스터수에 대해 k-means 알고리즘을 적용해서 평균 클러스터 내 분산이 최소인 
# 클러스터수로 클러스터링을 실시한 결과를 되돌려줌 
user.action.km <- ykmeans(user.action.pca, keys, "PC1", 3:6)
table(user.action.km$cluster)

# 제1주성분과 제2주성분 분포를 그려봄 : 표준화시킨 주성분 점수에 대한 
ggplot(user.action.km, 
       aes(x=PC1, y=PC2, col=as.factor(cluster), shape=as.factor(cluster))) + 
  geom_point()


## 클러스터별 평균 산출하기 
# 원래값이 들어있는 데이터셋에 클러스터 넘버 구한 것을 붙여줌 
user.action.f.filterd$cluster <- user.action.km$cluster

# 같은 클러스터그룹인 것들끼리 속성별 평균 구하기 
user.action.f.center <- ldply(lapply(sort(unique(user.action.f.filterd$cluster)),
                                     function(i) {
                                       x <- user.action.f.filterd[user.action.f.filterd$cluster == i,
                                                                  -ncol(user.action.f.filterd)]
                                       apply(x,2, function(d) mean(d))
                                                  }
                                     )
                              )

## 레이터 차트용 데이터 작성하기
install.packages("fmsb") # 레이터 차트 작성에 필요 패키지
library("fmsb")

# 레이터 차트에 맞게 데이터를 만들어주는 함수
createRadarChartDataFrame <- function(df) {
    df <- data.frame(df)
    dfmax <- apply(df, 2, max) + 1
    dfmin <- apply(df, 2, min) - 1
      as.data.frame(rbind(dfmax, dfmin, df))
}

# 상관이 높은 변수 제외하기 

df <- user.action.f.center[, -(ncol(user.action.f.center)-1)]
df.cor <- cor(df)
df.highly.cor <- findCorrelation(df.cor, cutoff=0.91) # 해석하기 쉬운 숫자가 되도록 직접 조절 

# cor 가 큰 변수 12개를 제외해서 18개 중 6개 변수 사용 
df.filterd <- df[,-df.highly.cor]

# 레이터 차트용 데이터 작성하기 
df.filterd <- createRadarChartDataFrame(scale(df.filterd))
names(df.filterd)


names(df.filterd) <- c("레벨", "지원횟수", "지원받은 횟수",
                       "보스를 쓰러뜨린 횟수", "싸움횟수", "게임이용횟수")

radarchart(df.filterd, seg=5, plty = 1:5, plwd =4, pcol=rainbow(5))
legend("topright", legend=1:5, col=rainbow(5), lty=1:5)지

## 클러스터별로 KPI 산출하기 (?)

user.action.f.filterd$user_id <- as.numeric(rownames(user.action.f.filterd))

user.action.kpi <- merge(user.action.f.filterd, mau, by="user_id")

ddply(user.action.kpi, .(cluster), summarize, arpu= round(mean(payment)), 
      access_days = round(mean(access_days)))


#########################################################################
#############   MeMo  ###################################################

# foreach() : %do% 이하의 반복문을 list 로 반환 

> x <- foreach(a=1:3, b=rep(10, 3)) %do% { a + b }
> x
[[1]]
[1] 11
[[2]]
[1] 12
[[3]]
[1] 13


ldply(foreach(day = dates, combine = rbind) %do% {  })

# list -> dataframe 변환, dates 값에 대해서 행결합으로 반복작업 


# sprintf 함수는 특정 변수를 여러번 활용하여 문자열을 만들 때 이용됩니다.
# read.csv(sprintf("%s/%s/%s/data.tsv", base.dir, app.name, day), header = T, sep = "\t", stringsAsFactors = F)
# 뒤에 나오는 3개 인자를 받아 %s 에 넣어줌. %s 는 인자가 string 일 때.

> sprintf("%s is %3$f or %f feet tall\n", "Sven",7.2,7.3)
[1] "Sven is 7.300000 or 7.200000 feet tall\n"
