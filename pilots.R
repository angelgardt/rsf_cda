library(tidyverse)

cols_to_select <- c("arr_code", "srch_code", "type", "semi_field",
                    "cat", "target", "set_size", "key_resp_6.keys", "key_resp_6.rt")
files <- c("bhvr/frol_pilot_shapka_2021_Sep_09_1514.csv",
           "bhvr/NIKFNS2_shapka_2021_Sep_24_1533.csv",
           "bhvr/alefn_shapka_2021_Sep_28_1933.csv",
           "bhvr/m_pilot_shapka_2021_Sep_22_1803.csv")

import_data <- function(files, cols) {
  x <- tibble()
  for (i in 1:length(files)) {
    read_csv(files[i]) %>% 
      select(cols) %>% 
      filter(!is.na(arr_code)) %>% 
      slice(-(1:16)) %>% 
      mutate(id = i) %>% 
      rename("key" = key_resp_6.keys,
             "rt" = key_resp_6.rt) %>% 
      bind_rows(x) -> x
  }
  return(x)
}

raw_data <- import_data(files, cols_to_select)

raw_data %>% str()
raw_data$key %>% unique()
apply(
  sapply(raw_data, is.na),
  2, sum
)


is_correct <- function(type, key) {
  ifelse(
    (type == "pre" & key == "p") | (type == "abs" & key == "a"),
    TRUE, FALSE
  )
}

raw_data %>% group_by(id, set_size, cat) %>% 
  mutate(is.correct = is_correct(type, key)) %>% 
  summarise(n = n(),
            correct = sum(is.correct),
            incorrect = sum(!is.correct),
            acc = correct / n) %>% View
  
raw_data %>% group_by(id, set_size, cat) %>%
  summarise(mean.rt = mean(rt, na.rm = TRUE),
            sd.rt = sd(rt, na.rm = TRUE)) %>% View
