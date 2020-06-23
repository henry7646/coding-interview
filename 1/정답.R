getwd()
.libPaths()
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

ORD <- read_csv("C:/~/1. 주문정보.csv", locale=locale('ko',encoding='euc-kr'))
COUPON <- read_csv("C:/~/2. 쿠폰주문정보.csv", locale=locale('ko',encoding='euc-kr'))
RANK <- read_csv("C:/~/3. 상품순위.csv", locale=locale('ko',encoding='euc-kr'))
REC <- read_csv("C:/~/4. 고객별 추천상품.csv", locale=locale('ko',encoding='euc-kr'))

#1.[1. 주문정보], [2. 쿠폰주문정보] 데이터를 활용하여 전체 고객 별 마지막 주문시 사용한 쿠폰번호를 구하는 코드를 작성하시오 (단, 할인율 20% 이하 쿠폰은 무시)
#할인율 계산 방법 = 쿠폰할인액 / 주문금액
#NA는 쿠폰을 사용하지 않은 주문

##고객별 최종 주문 일시
last_ord_time <- ORD %>%
  group_by(고객번호) %>%
  summarize(주문일시 = max(주문일시))

##주문정보와 쿠폰주문정보를 결합(공통키: 주문번호,고객번호, 방식:left_join)
ord_coupon <- ORD %>%
  left_join(COUPON,by = c("주문번호","고객번호")) %>%
  mutate(할인율 = 쿠폰할인액/주문금액) %>%
  filter(할인율 > 0.2 | is.na(할인율) == 1)

##고객별 최종 주문 시 사용한 쿠번번호
last_ord_coupon_no <- last_ord_time %>%
  inner_join(ord_coupon,by = c("고객번호","주문일시")) %>%
  select(고객번호,사용쿠폰번호) %>%
  rename(마지막주문쿠폰 = 사용쿠폰번호)

#2. 주어진 데이터를 활용하여, 다음과 같은 테이블을 만드는 코드를 작성하시오
#주어진 데이터가 전체 주문 건이라고 가정한다.
#주문회차 : 동일 고객이 몇 번째 주문 했는가.
#같은 날에 2번 이상 구매했어도, 주문회차는 하루에 1만큼만 증가한다. 즉 같은 날 주문한 회차는 모두 동일하게 처리할 것.
#주문간격: 동일 고객의 직전 주문과의 주문일 차이
#주문간격이 소수로 나오는 경우에는 반올림 하지 말고 그대로 둘 것.

##주문정보에 주문일자,주문회차,주문간격 추가
ord_edit <- ORD %>%
  mutate(주문일자 = date(주문일시)) %>%
  group_by(고객번호) %>%
  mutate(주문회차 = dense_rank(주문일자)) %>%
  arrange(고객번호,주문일시) %>%
  group_by(고객번호) %>%
  mutate(주문간격 = difftime(주문일시,lag(주문일시,1), units = "days"))

##주문번호,고객번호,주문일자,주문회차,주문간격 선택
ord_rank_interval <- ord_edit %>%
  select(주문번호,고객번호,주문일자,주문회차,주문간격)

#3. 고객전략팀에서는 상품 추천 알고리즘을 활용하여, 고객들에게 PUSH 캠페인을 진행한다.
#고객당 총 5개의 상품을 추천해주는데, 알고리즘에 5개의 상품이 다 매칭되지 않는 경우에는 NA가 쌓인다.
#이 경우에는 팀 내부에서 정한 상품 순위를 바탕으로, 높은 순위에 rank되어 있는 상품을 대체 추천해준다.
#[3. 상품 순위] 데이터를 활용하여, [4. 고객별 추천상품] 데이터의 NA를 모두 채우는 코드를 작성하시오.
#단, 고객별로 동일한 상품을 2번 이상 추천해서는 안됨.

for(i in 1:nrow(REC)){
  if(any(is.na(REC[i,]))==TRUE){
    j<-1
    while(j<=length(which(is.na(REC[i,])))){
      REC[i,which(is.na(REC[i,]))[j]]=RANK$상품번호[j]
      j=j+1
    }
  }
  else{
    REC[i,]=REC[i,]
  }
}
  
#4.
#(1) 최근 7일치의 주문 데이터를 추출하여, 0시부터 24시까지 1시간 단위로 나눈 시간대별 누적 매출을 차트화 하시오
#(2) 최근 7일치의 주문 데이터를 추출하여, 0시부터 24시까지 1시간 단위로 나눈 시간대별 매출을 차트화 하시오

##주문정보와 쿠폰주문정보를 결합(공통키: 주문번호,고객번호, 방식:left_join)
##최근 7일치의 데이터만 추출
ord_coupon2 <- ORD %>%
  left_join(COUPON,by = c("주문번호","고객번호")) %>%
  mutate(할인율 = ifelse(is.na(쿠폰할인액/주문금액) == 1, 0, 쿠폰할인액/주문금액)) %>%
  filter(주문일시 >= ymd_hms("2019-12-28 00:00:00")) %>%
  mutate(판매액 = 주문금액*(1-할인율), 날짜 = as.factor(date(주문일시)), 시간대 = hour(주문일시))

##최근 7일간 날짜,시간대별 매출 및 누적매출
hourly_profit <- ord_coupon2 %>%
  group_by(날짜,시간대) %>%
  summarize(시간대별매출 = sum(판매액)) %>%
  group_by(날짜) %>%
  mutate(시간대별누적매출 = cumsum(시간대별매출))

##최근 7일간 시간대별 매출 추이 차트
hourly_profit_trend <- hourly_profit %>%
  ggplot(aes(시간대, 시간대별매출, color = 날짜)) +
  geom_line() +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,1), expand = c(0,0)) +
  ggtitle("시간대별 매출")

##최근 7일간 시간대별 누적매출 추이 차트  
hourly_cumulative_profit_trend <- hourly_profit %>%
  ggplot(aes(시간대, 시간대별누적매출, color = 날짜)) +
  geom_line() +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,1), expand = c(0,0)) +
  ggtitle("시간대별 누적매출")

##두 그래프 결합
recent_week_profit_trend <-
  ggarrange(hourly_profit_trend, hourly_cumulative_profit_trend, ncol = 1, nrow = 2, heights = c(2,2), align = "v") %>%
  annotate_figure(top = text_grob("최근 7일간 시간대별 매출 기록 (2019-12-28 ~ 2020-01-03)",face = "bold", size = 14))
