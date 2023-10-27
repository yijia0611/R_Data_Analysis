load("cars_df (1).RData")

cars_df %>% 
  glimpse()#can see structure of file



cars_clean_df <- cars_df %>%
  janitor::clean_names() %>%
  transmute(
    price = str_remove_all(vetelar, "\\D"),
    price_eur = str_remove_all(vetelar_eur, "\\D"),
    year = gsub(pattern = "/.*", replacement = "", evjarat),
    fuel = uzemanyag,
    distance = kilometerora_allasa,
    distance = str_remove_all(distance, "\\D")
    gear_type = sebessegvalto_fajtaja,
    gear_type = str_detect(gear_type , "Automate"),
    max_ppl = szallithato_szem_szama,
    max_ppl = str_remove_all(max_ppl, "\\D")
  ) %>% 
  mutate_at(vars(price_eur, year, distance, max_ppl),
            as.numeric)
cars_cleaned_df <- cars_df %>%
  janitor::clean_names() %>%
  transmute(
    # price = str_remove_all(vetelar, "\\D"),
    price_eur = str_remove_all(vetelar_eur, "\\D"),
    year = gsub(pattern = "/.*", replacement = "", evjarat),
    fuel = uzemanyag,
    distance = kilometerora_allasa,
    distance = str_remove_all(distance, "\\D"),
    gear_type = sebessegvalto_fajtaja, # gear_type = str_detect()
    gear_number = str_remove_all(gear_type,"\\D"),
    max_ppl = szallithato_szem_szama,
    max_ppl = str_remove_all(max_ppl, "\\D")
  )  %>%
  mutate_at(vars(price_eur, year, distance, max_ppl),
            as.numeric)

cars_cleaned_df %>% 
  visdat::vis_miss()

cars_cleaned_df %>% 
  replace_na(list(distance=0))#replace missing value with an explict value

cars_cleaned_df %>% 
  mutate(
    distance = ifelse(is.na(distance), median(distance, na.rm= TRUE), distance)
  )

cars_cleaned_df %>% 
  mutate_if(
    is.numeric, ~ ifelse(is.na(.),mean(., na.rm=TRUE), .)
  )
#test will have this function
#考试要�?
cars_cleared_df %>%
  mutate_if(
    is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .) #.: all the columns
  )



mice::mice(cars_cleaned_df) %>% 
  complete %>% 
  tibble()

m<- mean(cars_cleaned_df$price_eur, na.rm = T)
s<- sd(cars_cleaned_df$price_eur, na.rm = T )


ggplot(cars_cleaned_df) +
  aes(price_eur)+
  geom_density(aes(color ="True value"))+
  stat_function(geom="line", fun=dnorm, args=list(m, s))


tibble(mean =c(0,10))

replicate(10, rnorm(10), simplify = F)

#norm 
#'r';generate random values
#'d":distribution function
#p : probability function
#q :quantile function

rep(c(1, 2, 3), times = 2)
rep(c(1, 2, 3), each = 2)



#stimulation
tibble(mean = c(0, 10)) %>% 
  crossing(sd = c(0.1, 0.3)) %>%
  slice(rep(1:n(), each = 100)) %>%
  mutate(
    trajectory = map2(mean, sd, ~ rnorm(n = 10, mean =  .x, sd = .y)),
    emprirical_mean = map_dbl (trajectory, ~ mean(.))
  ) %>%
  ggplot() +
  aes(emprirical_mean) +
  geom_histogram() +
  facet_grid(sd ~ mean, scales = "free")
