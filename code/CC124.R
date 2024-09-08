#### CC124: Introduction to building machine learning models in R with mikropml (CC124)
####: URL https://www.youtube.com/watch?v=uBqzChxAreE&t=849s

source("code/CC121.R")
library(tidyverse)
library(broom)
library(ggtext)
library(Hmisc)

set.seed(121229)
composite %>% tail

install.packages("mikropml") ### Not work


sig_genera <- composite %>%
  nest(data = -taxonomy) %>% 
  mutate(test = map(.x =data, ~wilcox.test(rel_abund ~ srn, data = .x) %>% 
                      tidy)) %>% 
  unnest(test) %>%
  mutate(p.adust = p.adjust(p.value, method = "BH")) %>%
  filter(p.adust < 0.05) %>% 
  select(taxonomy, p.adust)

srn_genus_data <- composite %>% 
  select(group, taxonomy, rel_abund, srn) %>% 
  pivot_wider(names_from = taxonomy, values_from = rel_abund) %>% 
  select(-group) %>% 
  mutate(srn = if_else(srn, "srn", "healthy")) %>% 
  select(srn, everything())

run_ml(srn_genus_data,
       method = "glmnet",
       output_column = "srn",
       seed = 121229)




  

























