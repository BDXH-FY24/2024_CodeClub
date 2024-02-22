source("code/local_weather.R")

##CC235

tmax_prcp <- local_weather %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = T),
            prcp = sum(prcp, na.rm = T)) 
  
tmax_prcp %>% 
  filter(year != 1891 & year != year(today())) %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value))+
  geom_line()+
  facet_wrap(~name, ncol=1,scales = "free_y")+
  geom_smooth(se = F)




 
 
 scaled_tmax_prcp <- tmax_prcp %>% 
   mutate(tmax_tr = (tmax - min(tmax))/(max(tmax)-min(tmax)),
          tmax_mean = mean(tmax),
          tmax_max = max(tmax),
          prcp_tr = (prcp - min(prcp))/(max(prcp)-min(prcp)),
          prcp_mean = mean(prcp),
          prcp_max = max(prcp)
          )  
 
 tmax_plot <-  scaled_tamx_prcp %>% 
   ggplot(aes(x = year, y = tmax_tr))+
   geom_line(color = "blue")+
 
   scale_y_continuous(labels = seq(10, 30, 5),
                      breaks = (seq(10, 30, 5) -2.35)/17.5,
                      limits = (c(8,32) -2.35)/17.5
)
 
 
 tmax_plot +
   geom_line(aes(y = prcp_tr), color ="red")+
   scale_y_continuous(labels = seq(10, 40, 5),
                      breaks = (seq(10, 40, 5) -2.35)/17.5,
                      limits = (c(8,32) -2.35)/17.5,
                      name = "Average annual temperature",
                      sec.axis = sec_axis(trans = ~.,
                                          labels = seq(300, 2400, 300),
                                          name ="Total precipitation (mm)",
                                          breaks = (seq(300, 2400, 300) -162)/1298)
                                         )+
   theme(axis.title.y.left = element_text(color = "blue"),
         axis.title.y.right = element_text(color = "red")
   )
   
 tmax_prcp %>% 
   ggplot(aes(x=tmax, y=prcp, color =year))+
   geom_point()+
   geom_smooth(method = "lm")
 cor.test(tmax_prcp$tmax, tmax_prcp$prcp)
 cor.test(tmax_prcp$tmax, tmax_prcp$prcp, method = "spearman")

                       

 
 
 
 
 