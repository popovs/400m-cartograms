# 400m Cartograms
# Deep water fisheries manuscript

# 01 This Datacarpentry workshop is overall super helpful for any R GIS: http://www.datacarpentry.org/R-spatial-raster-vector-lesson/ 
# 02 This GitHub.io page goes through basic vector/shapefile plotting: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# 03 Plotting cartograms using R: http://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html 


# ----------------
# 01 INITIAL SETUP
# ----------------

# Setup

# Note this part only works if you are in RStudio. If not using RStudio, set wd manually to wherever this script is stored.
# Set working directory to directory this R script is stored in.
set_wd <- function() {
  if (!require(rstudioapi)) {
    install.packages("rstudioapi", repos = "http://cran.stat.sfu.ca/")
    require(rstudioapi)
  }
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  print(getwd())
}

set_wd()

# Install and load all other necessary packages
#if (!require(rgeos)) {
#  install.packages("rgeos", repos = "http://cran.stat.sfu.ca/")
#  require(rgeos)
#}
#if (!require(rgdal)) {
#  install.packages("rgdal", repos = "http://cran.stat.sfu.ca/")
#  require(rgdal)
#}
#if (!require(raster)) {
#  install.packages("raster", repos = "http://cran.stat.sfu.ca/")
#  require(raster)
#}
#if (!require(sf)) {
#  install.packages("sf", repos = "http://cran.stat.sfu.ca/")
#  require(sf)
#}
#if (!require(maps)) {
#  install.packages("maps", repos = "http://cran.stat.sfu.ca/")
#  require(maps)
#}
#if (!require(rasterVis)) {
#  install.packages("rasterVis", repos = "http://cran.stat.sfu.ca/")
#  require(rasterVis)
#}
#if (!require(stringr)) {
#  install.packages("stringr", repos = "http://cran.stat.sfu.ca/")
#  require(stringr)
#}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "http://cran.stat.sfu.ca/")
  require(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.stat.sfu.ca/")
  require(dplyr)
}
if (!require(cartogram)) {
  install.packages("cartogram", repos = "http://cran.stat.sfu.ca/")
  require(cartogram)
}
#if(!require(viridis)) {
#  install.packages("viridis", repos="http://cran.stat.sfu.ca/")
#  require(viridis)
#}

# ----------------
# 02 CSV READ AND DATAFRAME CREATION
# ----------------

fishing_data <- read.csv('FAOSAU_400m_country_gis.csv')

c1950 <- fishing_data[fishing_data$Year==1950,]
c1970 <- fishing_data[fishing_data$Year==1970,]
c1990 <- fishing_data[fishing_data$Year==1990,]
c2014 <- fishing_data[fishing_data$Year==2014,]

# ----------------
# 03 PREPARE GIS DATA
# ----------------

countries <- map_data("world")
countries <- countries[countries$region!='Antarctica',1:5] # remove Antarctica, drop irrelevant columns
names(countries)[names(countries) == "region"] <- "Country" # rename "region" column to "Country" so we can join the two datasets

# Join map data to fishing data
map_data_1950 <- merge(countries, c1950, by="Country", all=TRUE)
map_data_1970 <- merge(countries, c1970, by="Country", all=TRUE)
map_data_1990 <- merge(countries, c1990, by="Country", all=TRUE)
map_data_2014 <- merge(countries, c2014, by="Country", all=TRUE)

# Reorder the map data back into the correct order (merging messes that up)
map_data_1950 <- arrange(map_data_1950, order)
map_data_1970 <- arrange(map_data_1970, order)
map_data_1990 <- arrange(map_data_1990, order)
map_data_2014 <- arrange(map_data_2014, order)


# ----------------
# 04 PLOT
# ----------------

p1950 <- ggplot() + 
  # countries
  geom_polygon(data = map_data_1950, aes(
                                  fill = Catch,
                                  x = long, 
                                  y = lat,
                                  group = group
                                  )
               ) +
  # constrain proportions
  coord_fixed() +
  # countries outlines
  geom_path(data = countries, aes(
                                  x = long,
                                  y = lat,
                                  group = group),
            color = "white", size = 0.01
            )

plot(p1950)




cartogram(map_data_1950, "Catch", itermax=5)
