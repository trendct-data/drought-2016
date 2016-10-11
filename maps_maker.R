library(tidyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(rgdal)
library(sp)
library(maptools)
library(scales)
library(extrafont)
library(lubridate)

# Bring in the base layer of US state borders
usmap <- readOGR(dsn="us_shape", layer="statesp010g")

# Convert the shapefile into a dataframe that we can work with
usfort <- fortify(usmap, region="STATE_ABBR")

# Creating a shapefile specifically for Connecticut
ctfort <- filter(usfort, id=="CT")

# Creating a shapefile of all state shapes minus Connecticut
not_ct_fort <- filter(usfort, id!="CT")

# Compiling a list of the folders in the 'shapes' folder
shape_list <- list.files("shapes")

# What this loop does
# 1. Goes through every single folder in the 'shapes' folder
# 2. Imports the shapefile for that week
# 3. Places the U.S. drought shapefile
# 4. Then places the U.S. borders on top of it
# 5. Saves the image into the us_maps folder
# 6. For Connecticut, it also places the U.S. drought shapefile
# 7. Then places the not_ct_fort shapefile on top so it hides everything outside of Connecticut with white
# 8. Adds the ctfort layer so it has a defined black border
# 9. Then restricts the map view so it centers on Connecticut
# 10. Saves the image into the ct_maps folder
# 11. Repeats the process for U.S. and CT maps but adds additional details such as Title, date, and legend
# 12. Saves those images to the _gif folders

for (i in 1:length(shape_list)) {

file_name <- shape_list[i]
dsn_name <- paste0("shapes/", file_name)
layer_name <- gsub("_M", "", file_name)

droughtmap <- readOGR(dsn=dsn_name, layer=layer_name)
droughtfort <- fortify(droughtmap, region="DM")

gg <- ggplot() 
gg <- gg +  geom_polygon(data = droughtfort, aes(x=long, y=lat, group=group, fill=id), color = "black", size=0) 
gg <- gg +  geom_polygon(data = usfort, aes(x=long, y=lat, group=group, fill=total), color = "gray73", fill=NA, size=0.2) 
gg <- gg +  coord_map("polyconic", xlim=c(-125, -70), ylim=c(25.5, 48.5)) 
gg <- gg +  scale_fill_manual(name="Status", values = c("#FFFF00", "#FCD37F", "#FFAA00", "#E60000", "#730000"),
                      labels = c("Abnormally dry", "Moderate drought", "Severe drought", "Extreme drought", "Exceptional drought"))
gg <- gg + labs(x=NULL, y=NULL, 
                title=NULL,
                subtitle=NULL,
                caption=NULL)
gg <- gg +  theme_nothing(legend=F)
gg
file_path <- paste0("us_maps/", layer_name, ".png")
ggsave(gg, file=file_path, width=5, height=3, type="cairo-png")

## Connecticut only

gg <- ggplot() 
gg <- gg +  geom_polygon(data = droughtfort, aes(x=long, y=lat, group=group, fill=id), color = NA, size=0.5) 
gg <- gg +  geom_polygon(data = not_ct_fort, aes(x=long, y=lat, group=group, fill=total), color = "white", fill="white", size=1) 
gg <- gg +  geom_polygon(data = ctfort, aes(x=long, y=lat, group=group, fill=total), color = "gray73", fill=NA, size=0.5) 

gg <- gg +  coord_map("polyconic", xlim=c(-73.8, -71.6), ylim=c(40.9, 42.2)) 
gg <- gg +  scale_fill_manual(name="Status", values = c("#FFFF00", "#FCD37F", "#FFAA00", "#E60000", "#730000"),
                              labels = c("Abnormally dry", "Moderate drought", "Severe drought", "Extreme drought", "Exceptional drought"))
gg <- gg + labs(x=NULL, y=NULL, 
                title=NULL,
                subtitle=NULL,
                caption=NULL)
gg <- gg +  theme_nothing(legend=F) 

file_path <- paste0("ct_maps/", layer_name, ".png")
ggsave(gg, file=file_path, width=5, height=3, type="cairo-png")


}


## For GIFS
# This adds a headline and subhead of the date, as well as the legend

for (i in 1:length(shape_list)) {
  
  file_name <- shape_list[i]
  dsn_name <- paste0("shapes/", file_name)
  layer_name <- gsub("_M", "", file_name)
  the_date <- gsub("USDM_", "", layer_name)
  the_date <- ymd(the_date)
  ap_date <- paste0(month(the_date, label=T, abbr=F), " ", day(the_date), ", ", year(the_date))
  droughtmap <- readOGR(dsn=dsn_name, layer=layer_name)
  droughtfort <- fortify(droughtmap, region="DM")
  
  gg <- ggplot() 
  gg <- gg +  geom_polygon(data = droughtfort, aes(x=long, y=lat, group=group, fill=id), color = "black", size=0) 
  gg <- gg +  geom_polygon(data = usfort, aes(x=long, y=lat, group=group, fill=total), color = "gray73", fill=NA, size=0.2) 
  gg <- gg +  coord_map("polyconic", xlim=c(-125, -70), ylim=c(25.5, 48.5)) 
  gg <- gg +  scale_fill_manual(name="", values = c("#FFFF00", "#FCD37F", "#FFAA00", "#E60000", "#730000"),
                                labels = c("Abnormal", "Moderate", "Severe", "Extreme", "Exceptional"))
  gg <- gg + labs(x=NULL, y=NULL, 
                  title="Drought in the U.S.",
                  subtitle=ap_date,
                  caption="National Drought Mitigation Center (NDMC), \nthe U.S. Department of Agriculture (USDA), \n and the National Oceanic and Atmospheric Association (NOAA)")
  gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=13))
  gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=7, color="gray", margin=margin(t=10, r=80)))
  gg <- gg + theme(legend.position="top")
  gg <- gg + theme(axis.line =  element_blank(),
                   axis.text =  element_blank(),
                   axis.ticks =  element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank()) 
  gg
  file_path <- paste0("us_maps_gif/", layer_name, ".png")
  ggsave(gg, file=file_path, width=5, height=4, type="cairo-png")
  
  
  
  ## Connecticut only
  
  gg <- ggplot() 
  gg <- gg +  geom_polygon(data = droughtfort, aes(x=long, y=lat, group=group, fill=id), color = NA, size=0.5) 
  gg <- gg +  geom_polygon(data = not_ct_fort, aes(x=long, y=lat, group=group, fill=total), color = "white", fill="white", size=.5) 
  gg <- gg +  geom_polygon(data = ctfort, aes(x=long, y=lat, group=group, fill=total), color = "gray73", fill=NA, size=0.5) 
  gg <- gg +  coord_map("polyconic", xlim=c(-73.8, -71.6), ylim=c(40.9, 42.2)) 
  gg <- gg +  scale_fill_manual(name="", values = c("#FFFF00", "#FCD37F", "#FFAA00", "#E60000", "#730000"),
                                labels = c("Abnormal", "Moderate", "Severe", "Extreme", "Exceptional"))
  gg <- gg + labs(x=NULL, y=NULL, 
                  title="Drought in Connecticut",
                  subtitle=ap_date,
                  caption="National Drought Mitigation Center (NDMC), \nthe U.S. Department of Agriculture (USDA), \n and the National Oceanic and Atmospheric Association (NOAA)")
  gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=13))
  gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=7, color="gray", margin=margin(t=10, r=80)))
  gg <- gg + theme(legend.position="top")
  gg <- gg + theme(axis.line =  element_blank(),
                   axis.text =  element_blank(),
                   axis.ticks =  element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank()) 
  gg
  
  file_path <- paste0("ct_maps_gif/", layer_name, ".png")
  ggsave(gg, file=file_path, width=5, height=4, type="cairo-png")
}



