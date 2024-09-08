## CC122
source("code/CC121.R")
library(broom)
library(Hmisc)
library(ggtext)

sig_genera <- composite %>%
  nest(data = -taxonomy) %>% 
  mutate(test = map(.x =data, ~wilcox.test(rel_abund ~ srn, data = .x) %>% 
                      tidy)) %>% 
  unnest(test) %>%
  mutate(p.adust = p.adjust(p.value, method = "BH")) %>%
  filter(p.adust < 0.05) %>% 
  select(taxonomy, p.adust)
  

composite %>% 
  inner_join(sig_genera, by = "taxonomy") %>%
  mutate(rel_abund = (rel_abund) * 100 + 1/20000,
         taxonomy = str_replace(taxonomy, "(.*)", "*\\1*"),
         taxonomy =str_replace(taxonomy, "\\*(.*)_unclassified\\*",
                               "Unclassified<br>*\\1*"),
         srn = factor(srn, levels = c(T, F))) %>% 
  ggplot(aes(x = rel_abund, y =taxonomy,color=srn, fill=srn))+
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8,
                                              jitter.width = 0.4),
              shape =21)+
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int=0.5),
               geom = "pointrange",
               position = position_dodge(width = 0.8),
               color="black",
               show.legend = F)+
  theme_classic()+
  theme(axis.text.y=element_markdown())+
  scale_color_manual(NULL,
                     breaks = c(F, T),
                     values = c("gray", "dodgerblue"),
                     labels =c("Healthy", "SRN"))+
scale_fill_manual(NULL,
                   breaks = c(F, T),
                   values = c("gray", "dodgerblue"),
                   labels =c("Healthy", "SRN"))+
  labs(x = "Relatvie abundance (%)",
       y = NULL)
  scale_x_log10()




ggsave("figure/significant_genera.tiff", width = 6, height = 4)



















