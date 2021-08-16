.libPaths()
.libPaths("C:/R/library")
.libPaths()
setwd("C:/R/public")

food_waste=read.csv("C:/R/public/FOOD_WASTE.csv")
short_term_frgn=read.csv("C:/R/public/SHORT_TERM_FRGN.csv")

#1. 음식물쓰레기 데이터 전처리
library(dplyr)

food_waste[(food_waste[,2]=="알수없음"),] # 알수없음 행 확인
food_waste=food_waste[,c(1,4,6,7,8)] # 필요한 열만 추출

summary(food_waste)
str(food_waste)

food_waste$base_date=substr(food_waste$base_date,1,7) #배출일자 월만 추출

result=food_waste %>% 
  group_by(base_date, emd_nm) %>% 
  summarise(sum(em_cnt), sum(em_g), sum(pay_amt)) #월별로 합계

write.csv(result,file="food_waste_month.csv", row.names=FALSE) #csv파일로 저장

#2. 단기체류 외국인 유동인구 전처리

summary(short_term_frgn)
str(short_term_frgn)

short_term_frgn$base_date=substr(short_term_frgn$base_date,1,7) #배출일자 월만 추출

short_term_frgn=short_term_frgn[,c(1,2,3,6,7)] # 필요한 열만 추출

#월별로 합치기
short_term_frgn_month=short_term_frgn %>% 
  group_by(base_date, emd_nm) %>% 
  summarise(sum(visit_pop_cnt))

colnames(short_term_frgn_month)[3]="visit_pop_cnt" #3열이름바꿔주기

summary(short_term_frgn_month[,3]) #이상치 범위 확인
IQR=87038.1-2410.2 #IQR=3분위-1분위
summary(short_term_frgn_month[,3])[5] + 1.5*IQR #upper=213980
summary(short_term_frgn_month[,3])[2] - 1.5*IQR #lower=-124531.6

#이상치 NA로 바꿔주기
short_term_frgn_month[,3] = ifelse(short_term_frgn_month[,3] < -124531.6 | short_term_frgn_month[,3] > 213980, NA, short_term_frgn_month[,3])

table(is.na(short_term_frgn_month[,3])) # 218개 NA

# 결측치 제거
library(dplyr)
short_term_frgn_month = short_term_frgn_month %>% filter(!is.na(visit_pop_cnt))

table(is.na(short_term_frgn_month[,3])) #결측치 모두 제거 확인

# 표준화 = standard scaler
install.packages("caret")
library(caret)

scale_model=caret::preProcess(short_term_frgn_month[,3],method=c("center","scale"))
short_frgn_month_stand=predict(scale_model, short_term_frgn_month)

round(sapply(short_frgn_month_stand[,3],mean),2) #평균이 0
round(sapply(short_frgn_month_stand[,3],sd),2) # 표준편차가 1

#합치기
library(dplyr)
merge=left_join(result, short_frgn_month_stand, by=c("base_date","emd_nm"))

table(is.na(merge[,6])) # 결측치 243개
merge = merge %>% filter(!is.na(visit_pop_cnt)) #NA값 제거
table(is.na(merge[,6])) # 결측치 모두 제거 확인

#상관계수
cor(merge[,c(3,4,5,6)])