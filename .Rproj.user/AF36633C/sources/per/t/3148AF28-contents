library(ggpubr)
library(ggpmisc)
library(ggthemes)
library(tidyverse)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()
mydata <- unsc_slct
setwd("C:/A/FINAL/pic")
# ggplot aes에 weight 파라미터 이용 
# or geom_smooth 에 mapping(aes(weight = population))
# 기온 ~ 자살률
# stat_poly_eq 위치는 0 ~ 1 사이 
c11 <- mydata %>% 
  ggplot(aes(x = temperature, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#D82148", size = 1.2) +
  # facet_wrap(~year) + 
  labs(x = "기온 (℃)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 7) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family = "notokr"))

# 개인소득 ~ 자살률
c12 <- mydata %>% 
  ggplot(aes(x = income_per_capita, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#FF5151", size = 1.2) +
  # facet_wrap(~year) +
  labs(x = "1인당 개인소득 (천원)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 7) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

# 이혼률 ~ 자살률
c13 <- mydata %>% 
  ggplot(aes(x = divorce_number_per_population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "red", size = 1.2) +
  # facet_wrap(~year) +
  labs(x = "이혼건수 (지역 인구 대비)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 7) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

# 인구수 ~ 자살률
c14 <- mydata %>% 
  ggplot(aes(x = population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1.2) +
  #  facet_wrap(~year) +
  labs(x = "인구 수 (만 명)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 7) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

# 건강생활실천율 ~ 자살률
c15 <- mydata %>% 
  ggplot(aes(x = healthy_life, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1.2) +
  #  facet_wrap(~year) +
  labs(x = "건강생활실천율 (%)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 7) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))
ggarrange(c11, c12, c13, c14, c15, nrow = 2, ncol = 3, 
          legend = "none")
ggsave("unfaceted_r_yearly.png", height = 13.5, width = 16)
getwd()


c11
ggsave("c11.png", height = 14.5, width = 16, units = "cm")
c12
ggsave("c12.png", height = 13.5, width = 16, units = "cm")
c13
ggsave("c13.png", height = 13.5, width = 16, units = "cm")
c14
ggsave("c14.png", height = 13.5, width = 16, units = "cm")
c15
ggsave("c15.png", height = 13.5, width = 16, units = "cm")