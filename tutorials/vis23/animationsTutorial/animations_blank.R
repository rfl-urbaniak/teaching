#install.packages( pkgs = "rgdal", 
#                  configure.args = c("--with-proj=/bin", "--with-gdal=/bin"),
#                  dependencies=TRUE)

# remove.packages("rlang")
# install.packages("rlang")
#devtools::install_github('thomasp85/gganimate')
# install.packages("gifski")

library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gganimate)
library(readr)
library(datasauRus)


#check out:
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/


#source:
#https://www.kaggle.com/datasets/louissebye/united-states-hate-crimes-19912017


#check out https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/




#REVEAL THROUGH TIME
hc <- read.csv("hate_crime.csv", header = TRUE)

hcYearPlot <- ggplot(hcYear, aes(x = ______, y = crimes))+
  geom_line()

hcYearPlot <- hcYearPlot + theme_fivethirtyeight()+
  ggtitle("Hate crimes 1991-2017")


hcYearAnim <- hcYearPlot + transition_reveal(year)

#a bit bumpy
animate(hcYearAnim, nframes = 28, fps  = 1, height = 450 )



animate(hcYearAnim, nframes = 112, fps  = 10, height = 450 )
anim_save("hcYear.gif")


#---------------------
library(gapminder)
dat = gapminder
head(dat)


plotgdp <-   ggplot(______) + 
  geom_point(aes(x = ______, y = ______, col = continent, size = pop), alpha = 0.8) + theme_tufte() + 
  theme(legend.position = "top") + guides(size = "none") + 
  labs(x = "GDP per capita" ,y = "life expectancy",  col = "") 

plotgdp


plotgdp + transition_time(______)



plotgdp + transition_time(year) +
  labs(title = "______ {frame_time}")


#a nicer way
#note conversion to factor and switching to transition_states (you don't want to auto-fill 
#change between printed integers)

plotgdp +
  geom_text(aes(x = min(gdpPercap), y = min(lifeExp), label = as.factor(year)) ,
            hjust=-2, vjust = -0.2, alpha = 0.2,  col = "gray", size = 20) +
  ggtitle("Life expectancy vs. GDP by continent")+
  transition_states(as.factor(______), state_length = 0)


anim_save("gdpContinents.gif")



### shifting scale


dat %>% filter(country %in% c("Poland", "Switzerland", "United States", "Somalia", "Bangladesh")) %>%
  ggplot(aes(x = year,  y = lifeExp, color = country)) + geom_point() + geom_line()
  theme_tufte() +
  transition_reveal(______)


  
  dat %>% filter(country %in% c("Poland", "Switzerland", "United States", "Somalia", "Bangladesh")) %>%
    ggplot(aes(x = year,  y = lifeExp, color = country)) + geom_point() + geom_line() + 
    labs(title = "Life expectancy through time", subtitle = "(selected countries)")+
    theme_tufte() +
    ylab("life expectancy")+
    transition_reveal(year)+
    view_follow()
  
  
  anim_save("expectancyPoland.gif")
  
  
  
#now barchart
  


dat2 <- dat %>% filter(country %in% c("Poland", "Switzerland", "United States", "Somalia", "Bangladesh", "Botswana",
                                      "Italy", "Japan", "India", "Israel", "United Kingdom", "West Bank and Gaza") )
  

dat2 %>% filter(year == 1972) %>% ggplot(aes(reorder(country, lifeExp), y = lifeExp, fill = country), alpha = .5) +
  geom_bar(stat = "identity", alpha = 0.3) +
  coord_flip()+theme_tufte()+xlab("country")+ylab("life expectancy")


dat2plot <- dat2 %>%  ggplot(aes(reorder(country, ______), y = lifeExp, fill = ______), alpha = .5) +
  geom_bar(stat = "identity", alpha = 0.3) +
  coord_flip()+theme_tufte()+xlab("country")+ylab("life expectancy")+
  ggtitle("GDP accross time (selected countries)", subtitle = "______ {frame_time}")


dat2anim <-  dat2plot + transition_time(______)


animate(dat2anim)


anim_save("expectancyBarplot.gif")


mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp))

mean.temp


p <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )

p



p + transition_states(______, wrap = FALSE) + shadow_mark()  +
  enter_grow() +
  enter_fade()



anim_save("temperature.gif")




#Now with points


ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_void() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')

anim_save("datasaurus.gif")



p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()+theme_tufte()



anim <- p + 
  transition_states(______,
                    transition_length = ______,
                    state_length = ______)


anim




anim <- p + 
  transition_states(______,
                    transition_length = ______,
                    state_length = ______) +
                    ease_aes('cubic-in-out')  #adds smoothing



anim + ease_aes(y = 'bounce-out')



anim + ease_aes(y = 'bounce-out') +  
  ggtitle("Species: {closest_state}")+ 
  enter_fade() + 
  exit_shrink()



anim_save("petals.gif")



