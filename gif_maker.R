# This script generates GIFs from images in the us_maps_gif and ct_maps_gif folder that were generated using the maps_maker.R scripts

library(dplyr)
library(magick)
library(extrafont)


the_list <- paste0("us_maps_gif/", list.files("us_maps_gif/"))

frames <- lapply(the_list, image_read)
animation <- image_animate(image_join(frames), fps=4)
#print(animation)
image_write(animation,"us_map.gif")


the_list <- paste0("ct_maps_gif/", list.files("ct_maps_gif/"))

frames <- lapply(the_list, image_read)
animation <- image_animate(image_join(frames), fps=4)
image_write(animation,"ct_map.gif")
