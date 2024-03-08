# waterfall plot in ggplot

# install.packages("waterfalls")
library(waterfalls)
install.packages("ggalluvial")
library(ggalluvial)



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