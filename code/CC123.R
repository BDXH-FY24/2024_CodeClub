#### CC123
library(tidyverse)
library(broom)
library(ggtext)

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
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";unclassified", "_unclassified"),
         taxonomy = str_replace_all(taxonomy, ";unclassified", ""),
         taxonomy = str_replace_all(taxonomy, ";$", ""),
         taxonomy = str_replace_all(taxonomy, ".*;", "")
         )  
  










  

