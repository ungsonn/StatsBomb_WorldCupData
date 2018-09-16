library(tidyverse)     # data tidying
library(StatsBombR)    # importing sb data
library(SBpitch)       # for creating pitch
library(RColorBrewer)  # figure colors
library(magick)        # applying logo

events <- StatsBombFreeEvents()


# ... ---------------------------------------------------------------------


data <- events %>% 
  filter(competition_id == "43" & 
           player.name == "Eden Hazard" &
           type.name == "Dribble")

data <- data %>% separate(location, c("location.spare", "x", "y"))
data$x <- as.numeric(as.character(data$x))
data$y <- as.numeric(as.character(data$y))

data$dribble.nutmeg
belgcolor <- c("#ED2939", "#FAE042")
names(belgcolor) <- c("FALSE", "TRUE")
colscale <- scale_color_manual(name = "dribble.nutmeg", values = belgcolor)

create_Pitch() + 
  geom_point(data = data, aes(x = x, y = y, color = is.na(dribble.nutmeg)), size = 3) + 
  theme(legend.position = "none") + 
  colscale + 
  geom_text(aes(x = 2, y = 77, label = data$player.name[1]), 
            hjust = 0, vjust = 0.5, size = 5) + 
  geom_text(aes(x = 2, y = 72, label = "World Cup 2018"), 
            hjust = 0, vjust = 0.5, size = 3) +
  geom_text(aes(x = 2, y = 68.5, 
                label = paste0("Dribbles: ", 
                               nrow(data))), 
            hjust = 0, vjust = 0.5, size = 3) + 
  geom_text(aes(x = 2, y = 65, 
                label = paste0("Nutmegs: ", 
                               sum(!is.na(data$dribble.nutmeg)))), 
            hjust = 0, vjust = 0.5, size = 3) + 
  annotation_custom(logo, xmin = 1, ymin = 1)

library(imager)
#install.packages("grid")
library(grid)

logo <- load.image("figures/statsbomb-logo.jpg")
logo <- rasterGrob(logo)



+ 
  ggsave(filename = "figures/plot.png", 
         width = 6.72, height = , dpi = 300)