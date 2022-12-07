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
lm_data %>% 
  ggplot(aes(x = year, y = r_squared)) +
  geom_point(aes(colour = variable)) +
  geom_line(aes(colour = variable), size = 0.7) +
  theme_gdocs() + scale_colour_brewer(label = data_label, palette = "Set2") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  geom_hline(aes(yintercept = ref), 
             colour = "red", size = 0.8) +
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
           y = 0.3, x = 2018, vjust = 2, colour = "red",
           size = 10, family = "notokr")
#+
  #guides(color=guide_legend(""), family = "notokr")
#  legend.position = "none")
setwd("C:/A/FINAL/pic")
ggsave("rsqred_yrly_unfaceted.png", height = 13.5, width = 16)
