
# Visualizing the drought in Connecticut and the United States

This repository contains source shapefiles and visualization scripts for the Trend CT story:

*   [How the drought has spread in Connecticut](http://trendct.org/2016/10/11/how-the-drought-has-spread-in-connecticut/) .

# Walkthroughs

* Visualizing the drought in Connecticut and the United States - [[HTML](http://trendct-data.github.io/drought-2016)] [[RMarkdown](https://github.com/trendct-data/drought-2016/blob/master/index.Rmd)] 

### The Data

The data used in this analysis is from [Drought Monitor](http://droughtmonitor.unl.edu/Home.aspx) via partnership between the National Drought Mitigation Center at the University of Nebraska-Lincoln, the United States Department of Agriculture, and the National Oceanic and Atmospheric Administration.

### What's in this repo

* `ct_maps` **folder** - Folder with programatically generated maps of the drought in Connecticut.
* `ct_maps_gif` **folder** - Folder with programatically generated maps of the drought in Connecticut (with additional titles, subtitles, and legend) to be used to create a GIF with.
* `us_maps` **folder** - Folder with programatically generated maps of the drought in the United States.
* `us_maps_gif` **folder** - Folder with programatically generated maps of the drought in the United States (with additional titles, subtitles, and legend) to be used to create a GIF with.
* `shapes` **folder** - Folders of 2016 weekly shapefiles from the Drought Monitor's [GIS Data Archive](http://droughtmonitor.unl.edu/MapsAndData/GISData.aspx)
* `us_shape` **folder** - Base shapefile of the United States borders to layer on the drought shapefiles.
* `maps_maker.R` - Script to programatically generate .PNGs of the maps for each shapefile available. For production on TrendCT.org and for GIF creation. 
* `gif_maker.R` - Script to programatically generate CT and US specific GIFs using the generated .PNG files.
* `ct_map.gif` - GIF of the drought in Connecticut
* `us_map.gif` - GIF of the drought in the United States

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. [Read more](http://www.trendct.org/data).

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct-data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
0