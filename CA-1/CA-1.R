# Import libraries

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(gganimate)
library(av)
library(ggimage)
library(ggflags)


#library(extrafont)

library(lubridate)

library(sysfonts)
library(showtext)

# Useful article on colors with R : https://r-charts.com/colors/
font_add_google(name = "Lilita One", family = "title")
font_add_google(name = "Amiri", family = "subtitle")
font_add_google(name = "EB Garamond", family = "rest")
font_add_google("Nova Mono", "nova")

showtext_auto()

dataset = read_csv("test/unemployment_eu_gr.csv")


dataset = dataset %>%
  select(geo, TIME_PERIOD,OBS_VALUE,age) %>%
  rename(`Country Name` = "geo", Year = "TIME_PERIOD", value = "OBS_VALUE") %>%
  filter(Year >= 2000)

dataset$age = ifelse(dataset$age == "Y15-74", "Total Unemployment", "Youth Unemployment (<25)")

dataset$flags = ifelse(dataset$`Country Name` == "EL", "gr", "eu")

dataset$Year = as.Date(as.character(dataset$Year), format="%Y")

dataset$Year = year(dataset$Year)


animation = ggplot(dataset, aes(x = Year, y = value)) +
  geom_line(aes(color = `Country Name`, linewidth =2)) +
  facet_wrap(~age) +
  geom_point(aes(color = `Country Name`)) +
  geom_text(aes(x = Year + 2.2, y = value, label = if_else(age == "Total Unemployment",if_else((Year >= 2000) & (Year <=2009),"", sprintf("%.1f",value)),""), group = `Country Name`),
            size = 10, family = "nova", fontface = "bold") +
  geom_text(aes(x = Year + 2.2, y = value, label = if_else(age == "Youth Unemployment (<25)", sprintf("%.1f",value),""), group = `Country Name`),
            size = 10, family = "nova", fontface = "bold") +
  geom_text_repel(aes(x = Year + 2.2, y = value, label = if_else(age == "Total Unemployment",if_else((Year < 2000) | (Year >2009),"", sprintf("%.1f", value)),""), group = `Country Name`),
                  size = 10, family = "nova", fontface = "bold",box.padding = 0.5,direction = "y") +
  geom_flag(aes(x = Year, y = value, group = `Country Name`, country = flags,size = 30)) +
  scale_country() +
  scale_size(range = c(0, 30)) +
  labs(
    title = "<b>Unemployment in <span style = 'color: dodgerblue; font-weight:bold;'>Greece</span> and 
    <span style = 'color: #ffdd00; font-weight:bold;'>EU</span> over the years</b></span>",
    x = "",
    subtitle = "Year: {round(frame_along, digits = 0)}",
    caption = "stesiam, 2023 | Data: Eurostat",
    y = ""
  ) +
  scale_color_manual(values = c("dodgerblue1", "#ffdd00")) +
  scale_y_continuous(breaks = c(seq(0, 60, 10)), expand = expansion(add = 0.15, mult = 0.1)) +
  theme_classic(base_size = 50,
                base_family = "rest") +
  theme(
    plot.title = element_markdown(hjust = 0.5, family = "title", lineheight = 1),
    plot.subtitle = element_text(hjust = 0.5, family = "nova",size = 30),
    legend.position = "none",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "cornsilk"),
    panel.background = element_rect(fill = "cornsilk"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust=0.5)
  ) +
  transition_reveal(Year) +
  ease_aes("cubic-in-out") +
  enter_fade()

animation

animate(animation,
        duration = 40,
        width = 1920, height = 1080,
        renderer = av_renderer(),
        rewind = F,
        fps = 30,
        start_pause = 30,
        end_pause = 50)

anim_save("CA-1/CA-1.mp4")
