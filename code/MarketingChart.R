# waterfall plot in ggplot

# https://learnr.wordpress.com/2009/03/29/ggplot2_marimekko_mosaic_chart/

# install.packages("waterfalls")
library(waterfalls)
# install.packages("ggalluvial")
library(ggalluvial)
library(glue)


df <- data.frame(gr =  c(LETTERS[1:6]),
                 va = c(2000, 4000,2000,
                        -1500, -1000, -2500)
                ) 

new_df <- df %>% 
  rename(Group = gr,
         Values = va)

waterfall(new_df, calc_total = T, fill_by_sign = F, fill_colours = 2:8, rect_border = NA) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())+
  theme_minimal()+
  labs(y = "ldldl. dldld.  lsldkeieoidl",
       x = "sgeduwiivfhdflkdflif kddkdk")
  
  
#Another variation of the variable width column chart is a Marimekko chart or mosaic chart or eikosogram. In many marketing departments this chart is used to show proportion of a product market by region, and proportion of region by product. 
# Jon Peltier’s tutorial demonstrates the technique to create one in Excel. 


# df <- data.frame(segment = c("A", "B", "C",
#                              "D"), 
#                  segpct = c(40, 30, 20, 10),
#                  Alpha = c(44,40, 30, 25),
#                  Beta = c(25, 30, 30, 25),
#                  Gamma = c(20, 20, 20, 25),
#                  Delta = c(11,10, 20, 25))
# 

rm(list = ls())

library(glue)
library(tidyverse)
## Mekko chart
df <- data.frame(segment = c("A", "B", "C","D"),
                 segpct = c(40, 30, 20, 10),
                 Alpha = c(44,50, 30, 25),
                 Beta = c(25, 20, 30, 25),
                 Gamma = c(8, 9, 20, 25),
                 Delta = c(23,21, 20, 25))

df$xmax <- cumsum(df$segpct)   
df$xmin <- df$xmax - df$segpct
df$segpct <- NULL

dfm <- df %>%
  pivot_longer(cols = c(Alpha, Beta, Gamma, Delta), values_to = "value", names_to = "variables")

dfm1 <- dfm %>% 
  group_by(segment) %>% 
  mutate(ymax = cumsum(value),
         ymin = ymax - value,
         xtext = xmin + (xmax - xmin)/2,
         ytext = ymin + (ymax - ymin)/2) %>% 
  ungroup()

 

dfm1 %>% 
  ggplot( aes(ymin = ymin, ymax = ymax,
                   xmin = xmin, xmax = xmax, fill = variables))+
  geom_rect(color = "grey")+
  geom_text(aes(x = xtext, y = ytext,
                label = glue("{value}%")), size = 3) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  geom_text(aes(x = xtext, y = 103,
                label = paste("Product ", segment)), size = 3.5)+
  scale_fill_brewer(palette = "Set2")


  ggsave("figure/Mekko.png")
  
  
  
  
  
  
  
  # 
  # 
  # geom_text(aes(x = xtext, y = ytext,
  #               label = ifelse(segment == "A",
  #                              glue("{variables}-{value}%"),
  #                              glue("{value}%"))), size = 3.5) 





