## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(hrbrthemes)




## ------------------------------------------------------------------------
theme_set(theme_ft_rc(plot_title_size = 24,
                      subtitle_size = 18,
                      axis_text_size = 16,
                      caption_size = 12) +
          theme(text = element_text(size = 20),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14)))


## ------------------------------------------------------------------------
county_data <- read_csv("./nytimes-data/us-counties.csv")

selected_county_data <- county_data %>%
  filter((county == "Cook"& state == "Illinois") |
         (county == "Milwaukee" & state == "Wisconsin") |
         (county == "Waukesha" & state == "Wisconsin") |
         (county == "Dane" & state == "Wisconsin") |
         (county == "King" & state == "Washington") |
         (county == "Los Angeles" & state == "California") |
         (county == "San Francisco" & state == "California") |
         (county == "Wayne" & state == "Michigan") |
         (county == "New York City" & state == "New York") |
         (county == "Orleans" & state == "Louisiana") |
         (county == "Miami-Dade" & state == "Florida") |
         (county == "Dallas" & state == "Texas")) %>%
  mutate(county = fct_reorder(county, cases, max))


## ------------------------------------------------------------------------
county_colors <- gg_color_hue(length(levels(selected_county_data$county)))
names(county_colors) <- levels(selected_county_data$county)


## ------------------------------------------------------------------------
death_data <- selected_county_data %>%
  group_by(state, county) %>%
  mutate(diff = deaths - lead(deaths)) %>%
  filter(deaths > 0, deaths != 1 | diff != 0) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup()

synthetic <- generate_synthetic_data(1, c(1, 2, 3, 4), 2.3 * max(death_data$deaths)) %>%
  rename(deaths = value) %>%
  filter(n <= max(death_data$n))

synthetic_text <- synthetic %>%
  group_by(days_to_double) %>%
  summarize(x = last(n),
            y = last(deaths)) %>%
  mutate(text = c("Deaths double\nevery day",
                  "..every\n2 days",
                  "..every\n3 days",
                  "..every\n4 days"))

death_points <- death_data %>%
  group_by(county) %>%
  filter(n == max(n)) %>%
  ungroup()

p_00_county <- death_data %>%
  ggplot(aes(x = n, y = deaths)) +
  geom_line(data = synthetic, aes(group = days_to_double), alpha = 0.9, linetype = "dashed") + # synthetic
  geom_text(data = synthetic_text, aes(x = x, y = y, label = text), inherit.aes = FALSE, nudge_x = -0.5, size = 5) +
  geom_line(aes(color = county)) +
  geom_point(data = death_points, aes(color = county)) +
  geom_text_repel(data = death_points, aes(label = county, color = county), size = 7, hjust = 1, vjust = 1) +
  scale_y_log10(labels = scales::label_number_si()) +
  theme(legend.position = "None") +
  scale_color_manual(values = county_colors) +
  labs(x = "day",
       title = "County cumulative deaths",
       caption = "Source: https://github.com/nytimes/covid-19-data")




## ------------------------------------------------------------------------
cases_init <- 10

cases_data <- selected_county_data %>%
  filter(cases >= cases_init) %>%
  group_by(county, state) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup()

synthetic <- generate_synthetic_data(cases_init, c(1, 2, 3, 4), 2 * max(cases_data$cases)) %>%
  rename(cases = value) %>%
  filter(n <= max(cases_data$n))

synthetic_text <- synthetic %>%
  group_by(days_to_double) %>%
  summarize(x = last(n),
            y = last(cases)) %>%
  mutate(text = c("Casess double\nevery day",
                  "..every\n2 days",
                  "..every\n3 days",
                  "..every\n4 days"))

cases_points <- cases_data %>%
  group_by(county) %>%
  filter(n == max(n)) %>%
  ungroup()  
    
p_01_county <- cases_data %>%
  ggplot(aes(x = n, y = cases)) +
  geom_line(data = synthetic, aes(group = days_to_double), alpha = 0.9, linetype = "dashed") + # synthetic
  geom_text(data = synthetic_text, aes(x = x, y = y, label = text), inherit.aes = FALSE, nudge_x = -0.5, size = 5) +
  geom_line(aes(color = county)) +
  geom_point(data = cases_points, aes(color = county)) +
  geom_text_repel(data = cases_points, aes(label = county, color = county), size = 7, hjust = 1, vjust = 1) +
  scale_y_log10(labels = scales::label_number_si()) +
  theme(legend.position = "None") +
  scale_color_manual(values = county_colors) +
  labs(x = "day",
       title = "County cumulative cases",
       caption = "Source: https://github.com/nytimes/covid-19-data")

