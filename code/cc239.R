source("code/local_weather.R")
library(ggtext)
library(htmltools)
library(slider)
#### CC239

## Slider function
x <- 1:10
slide(x, ~.x,.before = 2)
slide(x, ~.x,.after = 2, .complete = TRUE)

slide_dbl(x, ~sum(.x),.before = 2, .complete = TRUE)
tibble(x = 1:10) %>% 
  mutate(totla = slide_dbl(x, ~sum(.x), .before = 2, .complete = T))


drought_data <- local_weather %>% 
  select(date, prcp) %>% 
  mutate(prcp = if_else(is.na(prcp),0, prcp )) %>%
  arrange(date) %>% 
  mutate(window_prcp = slide_dbl(prcp, ~sum(.x),
                                 .before = 99, 
                                 .complete = T)) %>% 
  drop_na(window_prcp) %>%
  mutate(start = date - 29) %>% 
  select(start, end = date, window_prcp) %>%
  mutate(end_month = month(end),
         end_day = day(end),
         end_year = year(end)) %>% 
  group_by(end_month, end_day) %>% 
  mutate(threshold = quantile(window_prcp, probs = 0.05)) %>% 
  ungroup() #%>% 
  #filter(window_prcp < threshold) 
 
  
drought_line <- drought_data %>% 
  select(end_month, end_day, threshold) %>% 
  distinct() %>% 
  mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}")))
  

  
  drought_data %>% 
    mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}"))) %>% 
    select(-start, -end) %>% 
    mutate(is_drought_year = end_year == 2012,
           end_year = fct_reorder(factor(end_year), is_drought_year)) %>% 
    ggplot(aes(x = fake_date, y = window_prcp, group = end_year,
               color = is_drought_year))+
    geom_line(show.legend = F)+
    geom_line(data =  drought_line,aes(x = fake_date, y = threshold),
              inherit.aes = F,
              color = "red")+
    scale_color_manual(breaks = c(T, F),
                       values = c("dodgerblue", "gray"))+
    scale_x_date(date_breaks = "2 months", date_labels = "%B")+
    labs(x. = NULL,
         y = "Total precipitation over previous 30 days (mm)",
         title = "The summer of <span style = 'color:dodgerblue'>2012 </span>had less precipitation than <span style = 'color: red'>95%</span> of previous year dating back\nto 1892")+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          plot.title = element_textbox_simple(margin = margin(b = 20)),
          plot.title.position = "plot"
          )
    
    
  ggsave("figure/Drought.png", width = 6, height = 4)  
    
  
  
  
  
  





























































































  