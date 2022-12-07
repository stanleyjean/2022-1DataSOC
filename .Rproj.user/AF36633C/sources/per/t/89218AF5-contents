library(tidyverse)
library(RColorBrewer)
display.brewer.all()
windowsFonts()
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)
library(showtext)
font_add_google("Noto Serif KR", "notokr")
showtext_auto()
suicide_rate <- mydata %>% select(region, year, suicide_rate)

#전체 지역의 연도별 평균 
mean_suicide_rate <- suicide_rate %>% group_by(year) %>%
  summarise(
    mean_rate = mean(suicide_rate)
  ) %>% mutate(region = "ALL") %>% relocate(region) %>% ungroup()

suicide_rate %>% 
  ggplot(aes(x = as_factor(year), y = suicide_rate,
             group = region, colour = region)) +
  geom_line() +
  geom_line(data = mean_suicide_rate,
            aes(x = as_factor(year), y = mean_rate), colour = "red", size = 1) +
  theme_gdocs() + scale_colour_hue()

#전체 평균 
mean_suicide_rate %>% 
  summarise(mean_rate = mean(mean_rate)) # 28.6 
var_data
#지역별 평균 나타냄 bar plot 
suicide_rate %>% 
  group_by(region) %>% 
  summarise(mean_rate = mean(suicide_rate)) %>% 
  ggplot(aes(x = reorder(region, -mean_rate)
             , y = mean_rate, fill = region))+
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 28.6, colour = "red", size = 1) +
  coord_cartesian(ylim = c(20, 40)) +
  geom_text(aes(label = str_c(mean_rate, "명"), y = mean_rate + 0.5),
            size = 6) +
  annotate(geom="text", label= "전체 지역 평균 : 28.6명", x = 13, y = 28.6, vjust = -0.5, colour = "red",
           size = 13, family = "notokr") +
  theme_tufte() + scale_fill_manual(values = mycolors) +
  labs(x = "", y = "자살률(인구 십 만명당)", fill = "지역") +
  theme(axis.title = element_text(size = 25, family = "notokr"),
        axis.text = element_text(family = "notokr", size = 25),
        legend.title = element_text(size = 25, family = "notokr"),
        legend.text = element_text(size = 25, family = "notokr")) 