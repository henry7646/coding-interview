getwd()
setwd("C:/R/Market Kurly")
rm(list = ls())
library(data.table)
library(dplyr)
library(bit64)

#현재 경로에서 data.table 패키지의 fread 함수로 외부 csv 파일 불러오기
#주문일자별 평균매출 구하기
#"주문일자:평균매출" 칼럼 (문자열) 생성
daily_profit <- fread("1. 주문정보.csv", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  mutate(주문일자 = as.Date(주문일시)) %>%
  group_by(주문일자) %>%
  summarize(평균매출 = mean(주문금액))

Daily_average_profit <-
  paste(daily_profit$주문일자, daily_profit$평균매출, sep = ": ")