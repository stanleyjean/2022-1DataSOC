library(ggcorrplot)
# region과 name, 임금상승률(2011 년 NA) 제거
cor_un <- var_data %>% select(-c(1:2, 11))
ggcorrplot::ggcorrplot(cor(cor_un), lab = T,
                       title = "UNSCALED")

ccc <- var_data %>% 
  filter(name != "2011") %>%
  select(-c(1:2)) %>% 
  select(c(9, 17))
ggcorrplot::ggcorrplot(cor(ccc), lab = T,
                       title = "UNSCALED")
                       
# 독립변수가 상관계수가 높은 것을 
# 쳐냈음 

#사회복지 전담공무원 20
#청년실업률 16
#실업륜 15
#고용률 12
#상용월평균임금 10
#1인당개인소득 9
#이혼건수 7
#주민등록인구 6
#기온 5
#건강생활실천율 4
names(var_data)
cor_un <- var_data %>% select(c(4:7, 9, 10, 12, 15, 16, 20, 19)) 
ggcorrplot::ggcorrplot(cor(cor_un), lab = T,
                       title = "UNSCALED")


# 자살률과 상관계수가 낮은 것을 쳐냈음 
names(var_data)
names(scaled_var)
#실업률 15 ------
#개인소득 9
#이혼건수 7 
#주민등록인구 6
#기온 5
#건강생활실천율 4 ------

cor_un <- var_data %>% 
  select(c(4:6, 7, 9, 19)) 
# UNSCALED Data의 correlation 이용, 유의수준까지 확인 0.05 기준 
ggcorrplot::ggcorrplot(cor(cor_un), lab = T,
                       title = "UNSCALED",
                       p.mat = ggcorrplot::cor_pmat(cor_un))

# 최종적으로 변수 추출 
# 최종 분석에 이용할 데이터 

unsc_slct <-  var_data %>% 
  select(c(1, 2, 4:6, 7, 9, 19)) %>% 
  rename(year = name) %>% 
  mutate(year = as_factor(year))

unsc_slct <- unsc_slct %>% mutate(
  population = population / 10000 # 인구수를 만명단위로 바꾸기 
)

getwd()
setwd("C:/A/FINAL/data")
save.image("usage.RData")
