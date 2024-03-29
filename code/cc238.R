source("code/local_weather.R")
library(ggtext)
library(htmltools)


## CC238

snow_data <- local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(cal_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-07-01")),
                             cal_year - 1,
                             cal_year)
         ) %>%
  select(month, snow_year, snow) %>% 
  filter(snow_year!= 1891 &  snow_year!= 2022)

  
snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ggplot(aes(x = snow_year, y = total_snow))+
  geom_line()

total_snow <- snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow =sum(snow)) %>% 
  filter(snow_year == 2021) %>% 
  mutate(total_snow = total_snow/10) %>% 
  pull(total_snow)


dummy_df <- crossing(snow_year = 1892: 2021,
                     month = 1:12) %>% 
  mutate(dummy = 0)



snow_data %>% 
  right_join(., dummy_df, by = c("snow_year", "month")) %>% 
  mutate(snow_dummy = if_else(is.na(snow), dummy, snow)) %>% 
  group_by(snow_year,month) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, 
                        levels = c(8:12, 1:7)),
         is_this_year =   snow_year == 2021) %>% 
  ggplot(aes(x = month, y = snow, group = snow_year, color =is_this_year))+
  geom_line(show.legend = F)+
  scale_color_manual(name = NULL,
                     breaks =c(T, F),
                     values = c("dodgerblue", "lightgray"))+
  scale_x_discrete(breaks = c(9,11,1,3,5),
                   labels = month.abb[c(9,11,1,3,5)],
                   expand = c(0,0))+
  scale_y_continuous(breaks = seq(0, 1500, 500),
                     labels = seq(0, 150,50))+
  labs(x = NULL, y = "Total monthly snow fall (mm)",
       title = glue("The <span style = 'color: dodgerblue'>snow year 2021</span> has a total of {total_snow} cm of snow"))+
  
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        plot.title.position = "plot",
        plot.title = element_markdown()
        
        )

ggsave("figure/snow_by_snow_year.png", width = 6 , height = 4)




























