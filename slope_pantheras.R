meter<-c(0,0,500,0,0,
         4200,4500,5800,5200,3000)
elevation<-c("Lower elevation", "Lower elevation", "Lower elevation","Lower elevation","Lower elevation",
             "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation", "Upper elevation")
species<-c("Lion", "Tiger", "Snow leopard","Leopard", "Jaguar")
a<-data.frame(meter, elevation, species)

library (ggplot2)
library (ggrepel)
library (dplyr)

MySpecial <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "bottom"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.bottom      = element_text(size=30)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=44, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

png(file = "pantheras_elevation.png", width = 950, height = 600)

p<-ggplot(a, aes(x = elevation, y = meter, group = species, color=species)) +
  geom_line(aes(color=species), size=2) + 
  geom_point(aes(color = species), size = 4) +
  theme_minimal(base_size = 18) + 
  scale_color_brewer(palette = "Dark2") +
  geom_text_repel(data = a %>% filter(elevation == "Lower elevation"), 
                aes(label = paste0(species, " : ", meter, "m")),  
                hjust = "left", 
                fontface = "bold", 
                size = 8, 
                nudge_x = -.6, 
                direction = "y") +
  geom_text_repel(data = a %>% filter(elevation == "Upper elevation"), 
                  aes(label = paste0(species, " : ", meter, "m")), 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 8, 
                  nudge_x = .7, 
                  direction = "y") +
  #geom_label(aes(label = meter), 
            # size = 2.5, 
            # label.padding = unit(0.05, "lines"), 
            # label.size = 0.0)+
  MySpecial +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  )

dev.off()

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("pantheras_elevation.png")
plot2<-image_annotate(plot, "Elevational gradient for the genus Panthera", 
                      color = "black", size = 35,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Data: IUCN Red List | Image credit: Caters News Agency | Visualization by @fblpalmeira", 
                      color = "gray", size = 13, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
snowleopard <- image_read("https://raw.githubusercontent.com/fblpalmeira/snowleopard/main/snowleopard_icon.png") 
out<-image_composite(plot3,image_scale(snowleopard,"x130"), gravity="north", offset = "+117+25")

image_browse(out)

# And overwrite the plot without a logo
image_write(out, "pantheras_elevation2.png")

