## CC241: https://www.youtube.com/watch?v=TtdyFGCUF0M&t=315s

source("code/local_weather.R")

tail(local_weather)

no_na_no_zero <- local_weather[ (!is.na(local_weather$prcp) & 
                                   !is.na(local_weather$snow)) &
                 local_weather$snow > 0,]

no_na_no_zero
cor.test(no_na_no_zero$prcp, no_na_no_zero$snow)
cor.test(~prcp+snow, data = no_na_no_zero)

no_nas <- drop_na(local_weather)
no_nas_no_zero <- dplyr::filter(no_nas, no_nas$snow > 0)

library(tidyverse)

no_nas_no_zero <- local_weather |>
  drop_na()|>
  dplyr::filter(snow > 0) 

cor.test(~prcp+snow,data = no_nas_no_zero)

local_weather %>% 
  drop_na() %>% 
  dplyr::filter(snow > 0) %>% 
  cor.test(~prcp+snow, data = .)

local_weather %>% 
  drop_na() %>% 
  dplyr::filter(snow > 0) %>% 
  cor.test( prcp, snow, data = .)

local_weather %>% 
  drop_na() %>% 
  dplyr::filter(snow > 0) %$% 
  cor.test( prcp, snow, data = .)

local_weather %>% 
  drop_na() %>% 
  dplyr::filter(snow > 0) %T>% 
  print() %>% 
  summarize(total_prcp = sum(prcp))

plate <- 1:96 %>% 
  matrix(nrow = 8, ncol = 12) %>% 
  set_colnames(LETTERS[1:12]) %>% 
  set_rownames(1:8 )%>% 
  divide_by(10) %>% 
  as.data.frame() %>% 
  magrittr::extract2("D")

clean_data <- local_weather %>% 
  drop_na() %>% 
  dplyr::filter(snow > 0)

local_weather %<>% 
  drop_na() %>% 
  dplyr::filter(snow > 0)










