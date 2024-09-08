#### CC123: need to redo and learn
source("code/CC121.R")
library(tidyverse)
library(broom)
library(ggtext)
library(Hmisc)

set.seed(121229)


shared <- read_tsv("raw_data/raw_dataCC077_series/minimalR-raw_data-0.3/baxter.subsample.shared",
                   col_types = cols(Group = col_character(),
                                    .default = col_double())) %>% 
  rename_all(tolower) %>% 
  select(group, starts_with("otu")) %>% 
  pivot_longer(-group, names_to = "otu", values_to = "count") 


taxonomy <- read_tsv("raw_data/raw_dataCC077_series/minimalR-raw_data-0.3/baxter.cons.taxonomy") %>%
  rename_all(tolower) %>% 
  select(otu, taxonomy) %>% 
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy,"\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";unclassified","_unclassified"),
         taxonomy = str_replace_all(taxonomy, ";unclassified", ""),
         taxonomy = str_replace_all(taxonomy, ";$", ""),
         taxonomy = str_replace_all(taxonomy, ".*;", "")
         
  ) 



metadata <- read_tsv("raw_data/raw_dataCC077_series/minimalR-raw_data-0.3/baxter.metadata.tsv",
                     col_types = cols(sample = col_character())) %>% 
  rename_all(tolower) %>% 
  rename(group = sample) %>% 
  mutate(srn = dx_bin == "Adv Adenoma" | dx_bin == "Cancer",
         lession = dx_bin == "Adv Adenoma" | dx_bin == "Cancer") 


composite <- inner_join(shared, taxonomy, by = "otu") %>% 
  group_by(group,taxonomy) %>% 
  summarize_by(count = sum(count), .groups = "drop") %>% 
  group_by(group) %>% 
  mutate(rel_abund = count / sum(count)) %>% 
  ungroup() %>%
  select(-count) %>% 
  inner_join(., metadata, by = "group")

### cc123

get_sens_spec <- function(threshold, score, actual, direction){ 
  # threshold <- 100
  # score <- test$score
  # actual <- test$srn
  # direction <- "greaterthan"
  
  predicted <- if(direction == "greaterthan"){
    score > threshold
  }else{
      score < threshold
  }
 
  tp <- sum(predicted & actual)
  tn <- sum(!predicted & !actual)
  fp <- sum(predicted & !actual)
  fn <-sum(!predicted & actual) 
  
  specificity <- tn / (tn+fp)
  sensitivity <- tp / (tp+fn)
  
  tibble("specificity" = specificity, 
         "sensititivity" = sensitivity)
  
  
}


get_roc_data <- function(x, direction){
  x <- test
  direction <- "greaterthan"
  
  threshold <- unique(test$score)
  map_dfr(.x = threshold, ~get_sens_spec(.x, x$score, x$srn,direction))

}

get_roc_data(test, "greaterthan")

get_sens_spec(100, test$score, test$srn, "greaterthan")


 
  
  




roc_data <- composite %>% 
  inner_join(sig_genera, by = "taxonomy") %>% 
  select(group, taxonomy, rel_abund, fit_result,srn) %>% 
  pivot_wider(names_from = taxonomy, values_from = rel_abund) %>% 
  pivot_longer(cols = -c(group,srn), names_to = "metric", values_to = "score") %>% 
  filter(metric == "fit_result") %>% 
  nest(data= -metric) %>% 
  mutate(direction = if_else(metric == "Anaerococcus ","lessthan","greaterthan")) %>% 
  mutate(roc_data = map2(.x =data,.y=direction,  ~get_roc_data(.x,.y))) %>% 
  unnest(roc_data) %>% 
  select(metric, specificity, sensititivity)


roc_data %>% 
  ggplot(aes(x =1-specificity, y =sensititivity, color =metric))+
  geom_line()+
  geom_abline(slope =1, intercept = 0, color = "gray")









