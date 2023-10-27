library(tidyverse)
library(readxl)
gdp <- read_excel("gdp.xlsx")
library(readxl)
gdp <- read_excel("~/RÊ¾·¶code/gdp.xlsx")
View(gdp)

gdp_df <- gdp %>%
  set_names("year", "industry", "product", "added_value") %>% 
  mutate(
    year = str_remove_all(year, ". year"),
    year = as.numeric(year)
  ) %>% 
  fill(year)

nace2_df <- gdp_df %>% 
  mutate(
    cat = str_starts(industry, "\\d"),
    cat2 = str_detect(industry, "="),
    letter = ifelse(!cat & cat2, industry, NA),
    letter = str_sub(letter, end = 1)
  ) %>% 
  fill(letter) %>% 
  filter(cat)

nace1 <- gdp_df %>% 
  mutate(
    cat = str_starts(industry, "\\d"),
    cat2 = str_detect(industry, "=")
  ) %>% 
  filter(!cat, cat2)

nace1 %>% 
  ggplot() + 
  aes(x = year, y = product, color = industry) + 
  geom_line()

nace1 %>% 
  select(-cat, -cat2) %>% 
  mutate(
    industry = str_remove_all(industry, "\\w=  ")
  ) %>% 
  pivot_longer(product:added_value) %>% 
  ggplot() +
  aes(x = year, y = value, color = industry) + 
  geom_line(show.legend = FALSE) + 
  facet_wrap(~ name)

nace1 %>% 
  select(-cat, -cat2) %>% 
  mutate(
    industry = str_remove_all(industry, "\\w=  ")
  ) %>% 
  filter(year == max(year)) %>% 
  mutate(
    industry = fct_reorder(industry, added_value)
  ) %>% 
  arrange(desc(industry))


nace1 %>% 
  select(-cat, -cat2) %>% 
  mutate(
    industry = str_remove_all(industry, "\\w=  ")
  ) %>% 
  filter(year == max(year)) %>% 
  mutate(
    value_to_product = added_value / product,
    industry = fct_reorder(industry, value_to_product)
  ) %>% 
  arrange(desc(industry)) %>% 
  filter(value_to_product != 1) %>% 
  na.omit() %>% 
  ggplot() + 
  aes(value_to_product, industry) + 
  geom_col() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent)

nace2_df %>% 
  group_by(letter) %>% 
  summarise(min = min(added_value), max = max(added_value), 
            mean = mean(added_value), sd = sd(added_value),
            median = median(added_value)
  )

nace2_df %>% 
  na.omit() %>% 
  aov(formula = added_value ~ letter) %>% 
  summary()

nace2_df %>% 
  na.omit() %>% 
  lm(formula = added_value ~ letter) %>% 
  summary()



