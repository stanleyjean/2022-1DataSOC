library(ggpubr)
library(ggpmisc)
library(ggthemes)
library(tidyverse)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()
library(RColorBrewer)
display.brewer.all()
windowsFonts()
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)
mydata <- unsc_slct
options(scipen = 999)
setwd("C:/A/FINAL/pic")
## ggscatter? 
mydata <- unsc_slct
p_value_year <- c(2011, 2014, 2016)
tt <- mydata %>% mutate(year = as.numeric(as.character(year)))
tt
# 기온 ~ 자살률
c21 <- mydata %>% filter(year == 2011 | year == 2014 | year == 2016) %>%
  ggplot(aes(x = temperature, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#D82148", size = 1) +
  facet_wrap(~year) + 
  labs(x = "기온 (℃)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  #scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_few() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 15, family = "notokr") +
  theme(axis.title = element_text(size = 60, face = "bold"),
        axis.text = element_text(size = 40),
        text = element_text(family = "notokr", size = 40),
        strip.text =  element_text(size = 50))

# 개인소득 ~ 자살률
c22 <- mydata %>% filter(year == 2011 | year == 2014 | year == 2016) %>%
  ggplot(aes(x = income_per_capita, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "#FF5151", size = 1) +
  facet_wrap(~year) +
  labs(x = "1인당 개인소득 (천원)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  # stat_cor(label.y = 43, size = 3) +
  #scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_few() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 15, family = "notokr") +
  theme(axis.title = element_text(size = 60, face = "bold"),
        axis.text = element_text(size = 40),
        text = element_text(family = "notokr", size = 40),
        strip.text =  element_text(size = 50))

# 이혼률 ~ 자살률
c23 <- mydata %>% filter(year == 2011 | year == 2014 | year == 2016) %>%
  ggplot(aes(x = divorce_number_per_population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "red", size = 1) +
  facet_wrap(~year) +
  labs(x = "이혼건수(지역 인구 대비)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #stat_cor(label.y = 43, size = 3) +
  #scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_few() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 15, family = "notokr") +
  theme(axis.title = element_text(size = 60, face = "bold"),
        axis.text = element_text(size = 40),
        text = element_text(family = "notokr", size = 40),
        strip.text =  element_text(size = 50))
names(unsc_slct)
# 인구수 ~ 자살률
c24 <- mydata %>% filter(year == 2011 | year == 2014 | year == 2016) %>%
  ggplot(aes(x = population, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1) +
  facet_wrap(~year) +
  labs(x = "인구 수(만 명)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  #scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_few() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 15, family = "notokr") +
  theme(axis.title = element_text(size = 60, face = "bold"),
        axis.text = element_text(size = 40),
        text = element_text(family = "notokr", size = 40),
        strip.text =  element_text(size = 50))

c25 <- mydata %>% filter(year == 2011 | year == 2014 | year == 2016) %>%
  ggplot(aes(x = healthy_life, y = suicide_rate)) +
  geom_point(aes(colour = region, size = population)) +
  geom_smooth(method = "lm", se = F, colour = "orangered", size = 1) +
  facet_wrap(~year) +
  labs(x = "건강생활실천율 (%)", y = "자살률(십 만명 당)", colour = "지역",
       size = "인구 수(만 명)") +
  #  stat_cor(label.y = 43, size = 3) +
  #scale_x_continuous(breaks = c(2011, 2020, 2)) +
  theme_few() + scale_colour_pander() +
  stat_poly_eq(label.y = 0.85, size = 15, family = "notokr") +
  theme(axis.title = element_text(size = 60, face = "bold"),
        axis.text = element_text(size = 40),
        text = element_text(family = "notokr", size = 40),
        strip.text =  element_text(size = 50))

ggarrange(c21, c22, c23, c24, c25, nrow = 2, ncol = 3, 
          legend = "none")
ggsave("whole_yearly.png", height = 16, width = 25, units = "cm")

c21
ggsave("c21.png", height = 17, width = 25, units = "cm")
c22
ggsave("c22.png", height = 17, width = 25, units = "cm")
c23
ggsave("c23.png", height = 17, width = 25, units = "cm")
c24
ggsave("c24.png", height = 17, width = 25, units = "cm")
c25
ggsave("c25.png", height = 17, width = 25, units = "cm")
getwd()
list.files()
setwd("C:/A/FINAL/pic")


library(ggthemes)
library(tidyverse)
library(ggpmisc)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()
setwd("C:/A/FINAL/data")
lm_data <- read.csv("lm_data.csv")
lm_data <- lm_data %>% as_tibble() %>% select(-X)
#lm_data %>% print(n = Inf)
data_label = c("기온 (℃)",
               "1인당 개인소득 (천원)",
               "이혼건수 (지역 인구 대비)",
               "전체 인구 수 (만 명)",
               "건강생활실천율 (%)")


lm_data <- lm_data %>% mutate(variable = as_factor(variable))
levels(lm_data$variable) <- data_label
ref = 0.3
lm_data %>% filter(year == 2011 | year == 2014 | year == 2016) %>% 
  ggplot(aes(x = year, y = r_squared)) +
  geom_point(aes(colour = variable), size = 5) +
#  geom_line(aes(colour = variable), size = 0.7) +
  theme_gdocs() + scale_colour_brewer(label = data_label, palette = "Set2") +
  #scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  geom_hline(aes(yintercept = ref), 
             colour = "red", size = 0.3) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) + 
  labs(x = "", y = expression(R^{2}), colour = "") +
  # scale_x_continuous(breaks = NULL) +
  # facet_wrap(~variable, labeller = as_labeller(nn)) +
  theme(axis.ticks.x = element_blank(),
        axis.title = element_text(size = 20, face = "bold", family = "notokr"),
        legend.position = "none",
        strip.text = element_text(family = "notokr", face = "bold", size = 20)) +
  facet_wrap(~variable) +
  annotate(geom="text", label = "0.3",
           y = 0.3, x = 2014, vjust = -1, colour = "red",
           size = 10, family = "notokr") +
  scale_x_continuous(breaks = c(2011, 2014, 2016)) +
  coord_cartesian(xlim = c(2011,2016))
#+
#guides(color=guide_legend(""), family = "notokr")
#  legend.position = "none")
setwd("C:/A/FINAL/pic")
ggsave("rsqred_yrly_unfaceted_00.png", height = 13.5, width = 16)

