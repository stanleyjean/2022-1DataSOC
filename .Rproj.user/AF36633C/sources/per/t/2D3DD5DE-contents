library(tidyverse)
unique(kr_data$feature)

data0 <- kr_data %>% 
  filter(str_detect(feature, "기온|스트레스|건강생활"))
data1 <- kr_data %>% 
  filter(str_detect(feature, "주민등록|이혼건"))
data2 <- kr_data %>%
  filter(str_detect(feature, "주관적 건|GRDP|개인소득|고용률|임금"))
data3 <- kr_data %>%
  filter(str_detect(feature, "실업률|수급자")) # 4 * 17
data4 <- kr_data %>%
  filter(str_detect(feature, "사회복지 전담|순이동|자살률")) # 3 * 17
extracted_data <- tibble()
extracted_data <- bind_rows(extracted_data, data0)
extracted_data <- bind_rows(extracted_data, data1)
extracted_data <- bind_rows(extracted_data, data2)
extracted_data <- bind_rows(extracted_data, data3)
extracted_data <- bind_rows(extracted_data, data4)
extracted_data <- extracted_data %>% arrange(region)
extracted_data <- extracted_data %>% mutate(
  region = as_factor(region),
  feature = as_factor(feature)
)

#세종시 제외 
extracted_data <- extracted_data %>% filter(region != "Sejong")

rm(data0, data1, data2, data3, data4)
# 전체 인구로 나누기는 전부 더한 다음에 
# 칼럼으로 구분해내어서 구하고 그걸 다시 추가해주는 방식으로 하면 됨 
# 변수를 column으로 변경 
var_data <- extracted_data %>%
  pivot_longer(cols = contains("20")) %>%
  pivot_wider(names_from = "feature", values_from = "value")

#수급자수 / 전체인구 
#사회복지전담공무원수 / 전체인구
#이혼건수 / 전체인구 
var_data[, 7]  # 이혼 건수
var_data[, 17] # 수급자 수
var_data[, 20] # 사회복지 전담 공무원 
var_data[, 6]  # 전체 인구 수 
var_data <- var_data %>%
  mutate(
    var_data[, 7]/var_data[, 6],
    var_data[, 17]/var_data[, 6],
    var_data[, 20]/var_data[, 6]
  )

var_data # 분석에 이용할 데이터 
## 변수이름 영어로 변경 

selected_korea_names <- colnames(var_data)
colnames(var_data) <- c("region", "year",
                           "temperature", "population",
                           "divorce_number_per_population",
                           "income_per_capita",
                           "suicide_rate")
colnames(var_data)
unsc_slct <- unsc_slct %>% mutate(
  population = population / 10000
)
kk <- colnames(unsc_slct)
kk
colnames(unsc_slct) <- c("region", "year", "healthy_life",
                         "temperature", "population",
                         "divorce_number_per_population",
                         "income_per_capita",
                         "suicide_rate")

suicid_rate


kr_data
