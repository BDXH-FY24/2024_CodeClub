source("code/local_weather.R")
library(ggtext)
library(htmltools)

# CC236

today_month <- month(today())
today_day <- day(today())
today_date <- ymd(glue("2020-{today_month}-{today_day}"))

pretty_labels <- c("prob_prcp" = "Probability of precipitation","mean_prcp"= "Average amount of \n precipitation by day (mm)","mean_event"= "Average amount of \n precipitation by event (mm)")



local_weather %>% 
  select(date, prcp) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  drop_na(prcp) %>% 
  group_by(month, day) %>% 
  summarise(prob_prcp = mean(prcp > 0),
          mean_prcp = mean(prcp),
          mean_event = mean(prcp[prcp > 0]),
          .groups  = "drop") %>% 
  ungroup() %>% 
  mutate(date = ymd(glue("2020-{month}-{day}"))) %>% 
  select(-month,-day) %>% 
  pivot_longer(cols =c(prob_prcp, mean_prcp, mean_event)) %>% 
  mutate(name = factor(name, levels = c("prob_prcp", "mean_prcp", "mean_event" ))) %>% 
  ggplot(aes(x =date, y=value))+
  geom_vline(xintercept = today_date, color = "red",linewidth = 2)+
  geom_line()+
  geom_smooth(se = F)+
  facet_wrap(~name, ncol = 1, scales = "free_y",
             strip.position = "left",
             labeller = labeller(name = pretty_labels))+
  scale_y_continuous(limits = c(0, NA),
                     expan = c(0,0))+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%B")+
  coord_cartesian(clip = "off")+
  labs(x = NULL,
       y = NULL)+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line()
        )
  

  ggsave("figure/prcp_prob_amount.png", width = 3, height = 7)
  
  
















  
  
