library(RCurl)


z_theme <- function() {
  theme_bw(base_size=9) +
    #Background and Grid formatting
    theme(panel.background=element_rect(fill="#000000", color="#000000")) +
    theme(plot.background=element_rect(fill="#000000", color="#000000")) +
    theme(panel.border=element_rect(color="#252525")) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    #Legend formatting
    theme(legend.background = element_rect(fill="#000000")) +
    theme(legend.text = element_blank()) +
    theme(legend.title= element_blank())+
    theme(legend.position="none")+
    #Axis & Title Formatting
    theme(plot.title=element_text(color="#D9D9D9", size=20, vjust=1.25)) +
    theme(plot.subtitle=element_text(size=12,color="#BDBDBD", vjust=0)) +
    theme(plot.caption=element_text(size=12,color="#BDBDBD", vjust=0)) +
    theme(axis.ticks=element_blank()) +
    theme(axis.text.x=element_text(size=14,color="#BDBDBD")) +
    theme(axis.text.y=element_text(size=14,color="#BDBDBD")) +
    theme(axis.title.x=element_text(size=16,color="#BDBDBD", vjust=0)) +
    theme(axis.title.y=element_text(size=16,color="#BDBDBD", vjust=1.25))
}

download.file("https://raw.githubusercontent.com/MohamedElashri/Hubble/main/Data/data.csv",
              destfile="data.csv",
              method="libcurl")

galaxies <- read.csv("data.csv")


#E1. Check head and nrow of galaxies






# Convert Distance Modulus to MegaParsecs
galaxies$distmpc<-10^(1+galaxies$mod0/5)/1e6
# Convert Parsecs to Kilometers
galaxies$dist<-galaxies$distmpc*3.085678e+13*1e6
galaxies$vgsr <- as.numeric(galaxies$vgsr)

#E2. select complete cases

galaxies <- galaxies[complete.cases(___________),]

galaxiesClose<-subset(galaxies,distmpc<=250&vgsr<=15000)


#E3. check how many galaxies in the subset in the subset



#E4. plto distmpc on y vs vgsr, coloring by velocity
ggplot(galaxiesClose,aes(y = _______________,x = _____)+
  geom_point(shape=".",aes(color=_____))+
  scale_color_gradientn(colours=c("white","orange","red","darkred"))+
  labs(title="The Expanding Universe",
       subtitle = "Close galaxies",
       y="Distance from Earth (MPc)",
       x="Velocity Away from Earth (km/s)",
       caption="theme created by /Mohamed Elashri")+
  z_theme()

  
#E5. This is the first stab at the easiest model building, use the subset  
ageModelClose  <- lm(dist~vgsr+0,data=________)


signif(ageModelClose$coefficients[1]/60/60/24/365.24, 5)



#E6 Now analogous plot for the whole subset, change subtitle as well



#E7 Build model, estimate age of universe again
ageModel  <- 

signif(__________, ____)



#E8. First plot with geom_smooth()



#E9. Second plot with geom_smooth()

