# Import libraries

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

options(repr.plot.width=5, repr.plot.height=5)


# Import data

dataset = read_csv("CA-1/unemployment_data.csv", skip = 3)

# Keep Greece's and OECD's values

dataset = dataset %>%
  filter(`Country Name` == "Greece" | `Country Name` == "OECD members")

# Erase not important variables

dataset = dataset %>%
  select(-`Country Code`, -`Indicator Code`, -`Indicator Name`)
  
# Clean data

dataset = dataset %>%
  tidyr::pivot_longer(!`Country Name`, names_to = "Year")

# Erase observations (rows) with NA values

dataset = na.omit(dataset)

dataset = dataset %>% as_tibble()

# Change Year var (char -> num)

dataset$Year = as.numeric(dataset$Year)

anim = ggplot(dataset, aes(x = Year, y = value)) +
  geom_line(aes(color = `Country Name`)) +
  geom_point(aes(color = `Country Name`)) +
  labs(
    title = "Unemployment in Greece over the years",
    subitle = "Percentage of unemployment people of total labor force",
    caption = "stesiam, 2023 | Data : World Bank"
  ) +
  theme_classic() +
  transition_reveal(Year) +
  ease_aes("linear") +
  enter_fade()

anim_save("CA-1/CA-1.gif", anim)
