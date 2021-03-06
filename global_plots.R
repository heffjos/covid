## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(hrbrthemes)
library(plotly)
library(datasets)
library(broom)
library(ggrepel)

theme_set(theme_ft_rc(plot_title_size = 24,
                      subtitle_size = 18,
                      axis_text_size = 16,
                      caption_size = 12) +
          theme(text = element_text(size = 20),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14)))


## ------------------------------------------------------------------------
global_data <- read_csv("./data/global_data.csv") %>%
  mutate(country = case_when(country == "Korea, South" ~ "S Korea",
                             country == "United Kingdom" ~ "UK",
                             TRUE ~ country))

countries <- c("S Korea", "Italy", "US", "UK", "Canada", "Mexico", "France", "India", "Colombia", "China")

countries_data <- global_data %>%
  filter(country %in% countries)


## ------------------------------------------------------------------------
confirmed_data <- countries_data %>%
  filter(confirmed > 10) %>%
  group_by(country) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, confirmed, max, .desc = TRUE))

point_data <- confirmed_data %>%
  group_by(country) %>%
  filter(n == last(n))


## ------------------------------------------------------------------------
country_summaries <- confirmed_data %>%
  group_by(country) %>%
  summarize(max_confirmed = max(confirmed),
            max_n = n() - first(n),
            init = min(confirmed)) %>%
  ungroup() %>%
  summarize(max_confirmed = max(max_confirmed),
            max_n = max(max_n),
            init = mean(init))

max_confirmed <- country_summaries$max_confirmed
max_n <- country_summaries$max_n

doubling_days <- seq(1, 4)

samples_to_max <- ceiling(log(max_confirmed, 2^(1/doubling_days)))

doubling_lines <- tibble(
  days_to_double = unlist(map2(doubling_days, samples_to_max, rep)),
)

doubling_lines <- doubling_lines %>%
  group_by(days_to_double) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  mutate(confirmed = 10 * 2^(1/days_to_double * n)) %>%
  filter(confirmed <= 2 * max_confirmed) %>%
  group_by(days_to_double) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  filter(n <= max_n)

doubling_text <- doubling_lines %>%
  group_by(days_to_double) %>%
  summarize(x = last(n),
            y = last(confirmed)) %>%
  mutate(text = c("Cases double\nevery day",
                  "..every\n2 days",
                  "..every\n3 days",
                  "..every\n4 days"))

p_00_country <- confirmed_data %>%
  ggplot(aes(x = n, y = confirmed, color = country)) +
  geom_line(data = doubling_lines, aes(x = n , y = confirmed, group = days_to_double), linetype = "dashed", inherit.aes = FALSE) +
  geom_text(data = doubling_text, aes(x = x, y = y, label = text), inherit.aes = FALSE, nudge_x = -0.5, size = 5) +
  geom_point(data = point_data) +
  geom_line() +
  geom_text_repel(data = point_data, aes(label = country), size = 7, vjust = 1, hjust = 1) +
  labs(x = "day",
       y = "Confirmed Cases",
       color = "",
       title = "Confirmed cases for selected countries",
       subtitle = "Date begins when cases >= 10",
       caption = "Source: https://github.com/CSSEGISandData/COVID-19") +
  scale_y_log10() +
  theme(legend.position = "none")
  




## ------------------------------------------------------------------------
p_01_country <- countries_data %>%
  arrange(country, date) %>%
  group_by(country) %>%
  filter(date == last(date)) %>%
  ungroup() %>%
  mutate(norm_deaths = deaths / (confirmed / 100)) %>%
  mutate(country = fct_reorder(country, norm_deaths, median)) %>%
  ggplot(aes(y = country, x = norm_deaths)) +
  geom_point(color = ft_cols$yellow) +
  expand_limits(xmin = 0) +
  labs(title = "Country deaths per 100 confirmed cases",
       subtitle = "Only selected countries shown",
       captions = "Src: git@github.com:CSSEGISandData/COVID-19.git",
       x = NULL)




## ------------------------------------------------------------------------
country_model_data <- countries_data %>%
  filter(confirmed != 0) %>%
  group_by(country) %>%
  mutate(n = 1:n()) %>%
  filter(n >= last(n) - 6) %>%
  mutate(n = seq(0, (n() - 1))) %>%
  ungroup() %>%
  select(country, n, confirmed.log, confirmed)


## ------------------------------------------------------------------------
country_models <- country_model_data %>%
  group_by(country) %>%
  summarize(lm_mod = list(lm(confirmed.log ~ n))) %>%
  mutate(tidied = map(lm_mod, tidy, conf.int = TRUE)) %>%
  unnest(tidied) %>%
  filter(term != "(Intercept)") %>%
  mutate(country = fct_reorder(country, estimate, max),
         estimate.scale = 10^(estimate),
         conf.low.scale = 10^(conf.low),
         conf.high.scale = 10^(conf.high),
         conf.high.scale = ifelse(conf.high.scale > 2.5, 2.5, conf.high.scale))

p_02_country <- country_models %>%
  filter(country %in% countries) %>% 
  ggplot(aes(y = country, x = estimate.scale)) +
  geom_errorbarh(aes(xmin = conf.low.scale, xmax = conf.high.scale, height = 0.3)) +
  geom_point(color = ft_cols$yellow) +
  labs(title = "Estimated days between doubling of confirmed cases",
       subtitle = "Only confirmed cases in the past week were modeled",
       x = "days") +
  scale_x_continuous(breaks = 2^(1/seq(1, 9)), minor_breaks = NULL, labels = seq(1, 9))




## ------------------------------------------------------------------------
deaths_data <- countries_data %>%
  filter(deaths > 10, country %in% countries) %>%
  group_by(country) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, deaths, max, .desc = TRUE))

deaths_point_data <- deaths_data %>%
  group_by(country) %>%
  filter(n == last(n))

deaths_summaries <- deaths_data %>%
  group_by(country) %>%
  summarize(max_deaths = max(deaths),
            max_n = n() - first(n),
            init = min(deaths)) %>%
  ungroup() %>%
  summarize(max_deaths = max(max_deaths),
            max_n = max(max_n),
            init = mean(init))

max_deaths <- deaths_summaries$max_deaths
max_n <- deaths_summaries$max_n

doubling_days <- c(seq(1, 4), 7)

samples_to_max <- ceiling(log(max_deaths, 2^(1/doubling_days)))

doubling_lines <- tibble(
  days_to_double = unlist(map2(doubling_days, samples_to_max, rep)),
)

doubling_lines <- doubling_lines %>%
  group_by(days_to_double) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  mutate(deaths = 10 * 2^(1/days_to_double * n)) %>%
  filter(deaths <= 2 * max_deaths) %>%
  group_by(days_to_double) %>%
  mutate(n = seq(0, n() - 1)) %>%
  ungroup() %>%
  filter(n <= max_n)

doubling_text <- doubling_lines %>%
  group_by(days_to_double) %>%
  summarize(x = last(n),
            y = last(deaths)) %>%
  mutate(text = c("Deaths double\nevery day",
                  "..every\n2 days",
                  "..every\n3 days",
                  "..every\n4 days",
                  "..every\n7 days"))

p_03_country <- deaths_data %>%
  ggplot(aes(x = n, y = deaths)) +
  geom_point(data = deaths_point_data, aes(color = country)) +
  geom_line(data = doubling_lines, aes(group = days_to_double), linetype = "dashed") +
  geom_line(aes(color = country)) +
  geom_text(data = doubling_text, aes(x = x, y = y, label = text), inherit.aes = FALSE, nudge_x = -0.5, size = 5) +
  geom_text_repel(data = deaths_point_data, aes(label = country, color = country), size = 7, vjust = 1, hjust = 1) +
  scale_color_hue() +
  labs(title = "Country deaths beginning at 10 deaths",
       caption = "Source: https://github.com/CSSEGISandData/COVID-19",
       y = "deaths Cases",
       x = "day",
       color = "") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_log10() +
  theme(legend.position = "None")

