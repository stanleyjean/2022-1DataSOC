library(ggpubr)
library(ggpmisc)
library(ggthemes)
library(tidyverse)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()
mydata <- unsc_slct
options(scipen = 999)
setwd("C:/A/FINAL/pic")
## ggscatter? 
mydata <- unsc_slct
# 기온 ~ 자살률
c1 <- mydata %>% 
  ggplot(aes(x = temperature, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#D82148", size = 1.2) +
  facet_wrap(~year) + 
  labs(x = "기온 (℃)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 5) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family = "notokr"))

# 개인소득 ~ 자살률
c2 <- mydata %>% 
  ggplot(aes(x = income_per_capita, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#FF5151", size = 1.2) +
  facet_wrap(~year) +
  labs(x = "1인당 개인소득 (천원)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 5) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

# 이혼률 ~ 자살률
c3 <- mydata %>% 
  ggplot(aes(x = divorce_number_per_population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "red", size = 1.2) +
  facet_wrap(~year) +
  labs(x = "이혼건수(지역 인구 대비)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 5) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))
names(unsc_slct)
# 인구수 ~ 자살률
c4 <- mydata %>% 
  ggplot(aes(x = population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1.2) +
  facet_wrap(~year) +
  labs(x = "인구 수(만 명)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 5) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

c5 <- mydata %>% 
  ggplot(aes(x = healthy_life, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1.2) +
  facet_wrap(~year) +
  labs(x = "건강생활실천율 (%)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_gdocs() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 5) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 10))

ggarrange(c1, c2, c3, c4, c5, nrow = 2, ncol = 3, 
          legend = "none")
ggsave("whole_yearly.png", height = 13.5, width = 16)


c1
ggsave("c1.png", height = 13.5, width = 16, units = "cm")
c2
ggsave("c2.png", height = 13.5, width = 16, units = "cm")
c3
ggsave("c3.png", height = 13.5, width = 16, units = "cm")
c4
ggsave("c4.png", height = 13.5, width = 16, units = "cm")
c5
ggsave("c5.png", height = 13.5, width = 16, units = "cm")
