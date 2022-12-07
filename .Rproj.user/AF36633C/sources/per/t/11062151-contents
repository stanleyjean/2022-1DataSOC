# this script is for preprocessing of the data 
#install.packages("hablar")
library(tidyverse)
library(readxl)
library(hablar) # convert 함수 사용 위해서 필요 
options(scipen = 999)

getwd()
setwd("C:/A/FINAL/data/raw")
filename <- list.files()
# 자료 불러와서 저장하기
# 2011년 ~ 2020년의 데이터임, 지역별 "전체" 데이터 자료를 갖고오는 것 
the_whole_data_of_all_vars <- tibble()

for(i in seq(length(filename))){
  temp <- read_excel(filename[i], col_names = TRUE) 
  temp <- temp %>% mutate(
    region = str_remove_all(filename[i], ".xls"),
  ) %>% relocate(region) %>% 
    select(-c(2:3)) %>% select(-contains("2021")) %>% 
    convert(dbl(contains("20"))) %>% 
    mutate(
      feature = `지표별(2)`
    ) %>% select(-c(2)) %>% relocate(region, feature)
  #assign(str_remove_all(filename[i], ".xls"), temp) : 지역명 변수가 필요할 시 돌리기 
  the_whole_data_of_all_vars <- bind_rows(the_whole_data_of_all_vars,
                                          temp)
}

# the_whole_data_of_all_vars : e - 지방지표에서 가져온 모든 데이터
#####################################################
#지역명 한글로 변경
###################################################
region_name <- read.csv("region_name.csv")
region_name_KR <- read.csv("region_name_KR.csv")
region_name <- region_name$x
region_name_KR <- region_name_KR$x


################################################
# kr_data == 지역명이 한글로 변경된 것 
kr_data <- the_whole_data_of_all_vars
for(i in seq(length(region_name))){
  mydata <- mydata %>% 
    mutate(
      region = ifelse(region == region_name[i],
                      region_name_KR[i],
                      region)
    )
}
kr_data
