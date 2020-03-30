library(dplyr)
library(tibble)
library(purrr)

generate_synthetic_data <- function(init, doubling_days, max_value) {
  samples_to_max <- ceiling(log(max_value, 2^(1/doubling_days)))
  
  doubling_lines <- tibble(
    days_to_double = unlist(map2(doubling_days, samples_to_max, rep)),
  )
  
  doubling_lines <- doubling_lines %>%
    group_by(days_to_double) %>%
    mutate(n = seq(0, n() - 1)) %>%
    ungroup() %>%
    mutate(value = init * 2^(1/days_to_double * n))
  
  return(doubling_lines)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}