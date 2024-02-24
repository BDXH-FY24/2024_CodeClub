source("code/local_weather.R")
library(ggtext)
library(htmltools)

# CC237

prcp_snow_annual <- local_weather %>%
  drop_na() %>% 
  filter(snow > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(prcp = sum(prcp),
            snow = sum(snow)) %>% 
  filter(year != 1891 & year != 2024)
  
  
  
prcp_snow_annual %>% 
  pivot_longer((-year)) %>% 
  ggplot(aes(x =year, y = value, color = year)) +
  geom_line()+
  facet_wrap(~name, ncol= 1, scales = "free_y")
  

prcp_snow_annual %>% 
  ggplot(aes(x =prcp, y= snow, color = year)) +
  geom_point()

cor.test(prcp_snow_annual$prcp, prcp_snow_annual$snow)

## Modeling data on daily level


prcp_snow_daily <- local_weather %>%
  drop_na() %>% 
  filter(snow > 0 & tmax <= 0) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1891 & year != 2024) 

snow_model <- lm(snow~tmax*prcp + 0, data =prcp_snow_daily)
summary(snow_model)

predict(snow_model, prcp_snow_daily)




prcp_snow_daily  %>% 
  mutate(predicted = predict(snow_model, prcp_snow_daily)) %>% 
  ggplot(aes(x = prcp, y = snow, color = tmax))+
  geom_point(color = "lightgray")+
  geom_smooth(aes(color = "simple"), 
              formula = "y~x+0", 
              method = "lm" ,se = F)+
 geom_segment(
              aes(color = "rule_of_thumb"),x = 0, y = 0, xend = max(prcp_snow_daily$prcp), yend = 10*max(prcp_snow_daily$prcp))+
  geom_smooth(aes(y = predicted, , color = "advanced"), se = F)+
  labs(x = "Total daily precipitation (mm)",
       y = "Total daily snow (mm)")+
  scale_color_manual(name = NULL,
                     labels = c("10:1 rule of thumb", "Simple model", "Advanced model"),
                     breaks = c("rule_of_thumb","simple", "advanced"),
                     values = c("black", "blue","red"))
  theme_classic()





cor.test(prcp_snow_daily$prcp, prcp_snow_daily$snow)


ggsave("figure/model_snow_ratio.png", width = 6, height = 4)







