setwd("C:/A/FINAL/data")
reg_data <- read.csv("lm_data_pvalue.csv")
library(tidyverse)
library(ggthemes)
unique(reg_data %>% filter(p_value < 0.05) %>% select(year))

lm_data %>% head
reg_data %>% head
length(reg_data$year)

# p value 유의수준 0.05로 잡았을 때 통계적으로 유의미한 
# 연도는 2011, 2014, 2016년도 뿐이었음 
reg_data

mean_suicide_rate %>%
  ggplot(aes(x = year, y = mean_rate, group = 1)) +
  geom_point(colour = "red", size = 0.7) +
  geom_line(colour = "red", size = 1) +
  labs(x = "", y = "자살률(인구 십 만명 당)") +
  theme_gdocs() + 
  theme(text = element_text(family = "notokr"),
        axis.title = element_text(face = 'bold', size = 20))
  
