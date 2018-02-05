# 400m Cartograms
# Deep water fisheries manuscript

# 01 This Datacarpentry workshop is overall super helpful for any R GIS: http://www.datacarpentry.org/R-spatial-raster-vector-lesson/ 
# 02 This GitHub.io page goes through basic vector/shapefile plotting: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# 03 Plotting cartograms using R: http://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html (outdated/no good?)
# 04 Awesome Africa cartogram animation: https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/ 

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
# Note I am in British Columbia so SFU is the closest CRAN mirror for me, might be a bit slower if you are far away.
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.stat.sfu.ca/")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.stat.sfu.ca/")
  require(rgdal)
}
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
#if (!require(cartogram)) {
#  install.packages("cartogram", repos = "http://cran.stat.sfu.ca/")
#  require(cartogram)
#}
#if(!require(viridis)) {
#  install.packages("viridis", repos="http://cran.stat.sfu.ca/")
#  require(viridis)
#}


if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "http://cran.stat.sfu.ca/")
  require(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.stat.sfu.ca/")
  require(dplyr)
}
if (!require(maptools)) {
  install.packages("maptools", repos = "http://cran.stat.sfu.ca/")
  require(maptools)
}
if (!require(cartogram)) {
  install.packages("cartogram", repos = "http://cran.stat.sfu.ca/")
  require(cartogram)
}
if (!require(broom)) {
  install.packages("broom", repos = "http://cran.stat.sfu.ca/")
  require(broom)
}


# ----------------
# 02 CSV READ AND DATAFRAME CREATION
# ----------------

fishing_data <- read.csv('FAOSAU_400m_country_gis.csv', stringsAsFactors = F)

names(fishing_data) <- c("NAME", "YEAR", "CATCH") # Rename "Country" column to "NAME" so we can merge our catch data with the future spatial data, using the NAME column to link between the two. Capitalizing Year and Catch to make things more consistent once merged.

#c1950 <- fishing_data[fishing_data$Year==1950,]
#c1970 <- fishing_data[fishing_data$Year==1970,]
#c1990 <- fishing_data[fishing_data$Year==1990,]
#c2014 <- fishing_data[fishing_data$Year==2014,]

# In the v near future: create one large csv with the following columns: Country NAME, CATCH1950, CATCH1970, CATCH1990, CATCH2014 to quickly recreate the fig w 4 maps

# Create loop to make datasets for each
years <- unique(fishing_data$YEAR) # List containing all the years

fishing_years <- list() # Empty list that will contain the future year-by-year dataframes
for (year in years){
  #print(year)
  dfname <- paste0("c", year) # Create dataframe names in the form: "c<year>"
  #print(dfname)
  fishing_years[[dfname]] <- data.frame(fishing_data[fishing_data$YEAR == year,]) # create and assign these new dataframes the above created dataframe names (dfname), then populate it with rows that contain the correct <year>. THEN, chuck all these dataframes into one fishing_years list. tbh not sure why the dataframes themselves are appearing, need to actually fix that later. 
  rm(dfname) # remove the floaters
  rm(year)
}

#rm(list=ls(pattern="c")) # Useful for deleting large accidental amounts of objects created by loops x_x

# ----------------
# 03 PREPARE GIS DATA
# ----------------

data("wrld_simpl") # Simple world dataset from maptools
#world <- wrld_simpl[wrld_simpl$NAME != "Antarctica",]
world <- wrld_simpl[wrld_simpl$NAME != "Antarctica",c("ISO3", "NAME", "REGION", "LON", "LAT")] # World GIS SpatialPolygonsDataFrame (spdf) from base R, minus Antarctica, minus irrelevant columns (hopefully subregion is actually irrelevant loooool)

# world2 <- spTransform(world, CRS("+proj=eqc +ellps=WGS84 +datum=WGS84 +lon_0=10 +no_defs")) # maybe eventually shift central meridian over to +10, so that Chukchi peninsula is not chopped off Russia. 

#<object>@proj4string # Check CRS of a Spatial*DataFrame object.

# NOW CREATE MERGE LOOP
# Merge catch data with GIS data
# Put each merged year/map dataset into large map_years spdf list
map_years <- list()
for (year in years) {
    #print(year)
    fishing_year <- get(paste0("c", year), fishing_years) # get c<year> from the fishing_years list of df's, so we can merge
    dfname <- paste0("map", year)
    map_years[[dfname]] <- merge(world, i, by="NAME", all=TRUE)
}



for (i in 1:length(fishing_years)){
  
}


map1950 <- merge(world, c1950, by="NAME", all=TRUE)

# Fill NAs with 0s
map1950$CATCH[is.na(map1950$CATCH)] <- 0
# Now fill all values < 1 with 1 so cartogram actually works (otherwise countries with zeros will just be missing)
map1950$CATCH[map1950$CATCH < 1] <- 1

# Now make the cartogram! FYI this will take FOREVER. Each iteration takes ~1 minute. 
carto1950 <- cartogram(map1950, "CATCH", itermax=50)
plot(carto1950)

# Create shapefiles directory
dir.create("Shapefiles")
# Save as shapefile
writeOGR(obj = carto1950, dsn = "Shapefiles", layer = "carto1950", driver = "ESRI Shapefile") # "dsn" argument isn't the clearest on the documentation, but for ESRI Shapefiles I believe it's just the directory you want to save it to.

# ----------------
# 04 PLOT
# ----------------

# Transform all from SpatialPolygonsDataFrame to ggplot-friendly dataframe
gg1950 <- tidy(carto1950) # This works
gg1950 <- tidy(carto1950, region="NAME") # But this throws me an error?? 
#gg1950 <- merge(gg1950, carto1950@data, by="WHAT DO I MERGE BY???") # No idea what the "id" numbers are after tidying. id number 103 is INDONESIA after some testing (see below). Whiiiiich means that the id number does not line up AT ALL with the original cart1950 dataset. HMM. 

p1950 <- ggplot() + 
  geom_polygon(data = subset(gg1950, id == 103), aes(
                #fill = id, 
                x = long,
                y = lat,
                group = group))
plot(p1950)


# ORIGINAL GGPLOT TESTING

#countries <- map_data("world")
#countries <- countries[countries$region!='Antarctica',1:5] # remove Antarctica, drop irrelevant columns
#names(countries)[names(countries) == "region"] <- "Country" # rename "region" column to "Country" so we can join the two datasets

# Join map data to fishing data
#map_data_1950 <- merge(countries, c1950, by="Country", all=TRUE)
#map_data_1970 <- merge(countries, c1970, by="Country", all=TRUE)
#map_data_1990 <- merge(countries, c1990, by="Country", all=TRUE)
#map_data_2014 <- merge(countries, c2014, by="Country", all=TRUE)

# Reorder the map data back into the correct order (merging messes that up)
#map_data_1950 <- arrange(map_data_1950, order)
#map_data_1970 <- arrange(map_data_1970, order)
#map_data_1990 <- arrange(map_data_1990, order)
#map_data_2014 <- arrange(map_data_2014, order)

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
