getwd()
setwd("C:/A/FINAL/data")
library(readxl)
library(hablar)
library(ggthemes)
library(tidyverse)
library(ggpmisc)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()

# 자살충동이유도 단순히 전국 기준으로 하기에 한계 생김 
reason <- read_excel("suicide_reason.xls") %>%
  convert(dbl(contains("20")))
reason <- reason %>% filter(row_number() <= 13) %>%
  select(-c(1:3, 5)) %>%
  filter(row_number() > 3 & row_number() < 13) %>%
  mutate(
    feature = 항목,
    feature = as_factor(feature)
  ) %>% select(-항목) %>% relocate(feature)
# install.packages("RColorBrewer")
library(RColorBrewer)
# 자살 충동을 겪은 사람들 중에서 
# 자살 생각을 한 이유 
levels(reason$feature) <- str_remove(reason$feature, "-")

reason %>%
  pivot_longer(cols = contains("20")) %>% 
  mutate(name = as_factor(name))  %>% group_by(name) %>%
  ggplot(aes(x = name, y = value, fill = reorder(feature, -value))) + 
  geom_bar(stat = "identity") +
  labs(x = "", y = "비율", fill = "자살 충동 이유", title = "") +
  geom_text(aes(label = str_c(value, "%")), position = position_stack(vjust = 0.5),
            size = 3) + 
  theme_tufte() +
  theme(axis.ticks = element_blank(), #axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15, family = "notokr"),
        axis.title = element_text(size = 25, family = "notokr"),
        legend.text = element_text(size = 15, family = "notokr"),
        legend.title = element_text(size = 20, family = "notokr"),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set3")
ggsave("suicide_reason")
#scale_brewwer

reason %>%
  pivot_longer(cols = contains("20")) %>% 
  mutate(name = as_factor(name)) 
