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

# Create loop to make datasets for each year
years <- unique(fishing_data$YEAR) # List containing all the years

fishing_years <- list() # Empty list that will contain the future year-by-year dataframes
for (year in years){
  #print(year)
  dfname <- paste0("c", year) # Create dataframe names in the form: "c<year>"
  #print(dfname)
  fishing_years[[dfname]] <- data.frame(fishing_data[fishing_data$YEAR == year,]) # create and assign these new dataframes the above created dataframe names (dfname), then populate it with rows that contain the correct <year>. THEN, chuck all these dataframes into one fishing_years list. 
  rm(dfname) # remove the floaters
  rm(year)
}

#rm(list=ls(pattern="c")) # Useful for deleting large accidental amounts of objects created by loops x_x

# ----------------
# 03 PREPARE GIS DATA
# ----------------

# data("wrld_simpl") # Simple world dataset from maptools
data(World, package="tmap") # Trying out suggestion on Github here (https://github.com/sjewo/cartogram/issues/7), using a simplified world map to make things a bit faster
wo <- gSimplify(World, 10000)
wrld_simpl <- SpatialPolygonsDataFrame(wo, World@data, match.ID=F)
rm(World)

world <- wrld_simpl[wrld_simpl$name != "Antarctica",c("iso_a3", "name", "continent")] # World GIS SpatialPolygonsDataFrame (spdf), minus Antarctica, minus irrelevant columns (hopefully subregion is actually irrelevant loooool)
world <- spTransform(world, CRS("+proj=eqc +ellps=WGS84 +datum=WGS84 +no_defs")) # Transform back to EPSG 4326 projection

#world2 <- spTransform(world, CRS("+proj=eqc +ellps=WGS84 +datum=WGS84 +lon_0=10 +no_defs")) # maybe eventually shift central meridian over to +10, so that Chukchi peninsula is not chopped off Russia. 
#<object>@proj4string # Check CRS of a Spatial*DataFrame object.

names(world) <- c("ISO3", "NAME", "REGION")

# NOW CREATE MERGE LOOP
# Merge catch data with GIS data
# Put each merged year/map dataset into large map_years spdf list
map_years <- list()
for (year in years) {
    #print(year)
    fishing_year <- get(paste0("c", year), fishing_years) # get c<year> from the fishing_years list of df's, so we can merge
    dfname <- paste0("map", year) # create spdf names in the form "map<year>"
    #print(dfname)
    map_years[[dfname]] <- merge(world, fishing_year, by="NAME", all=TRUE) # merge world dataset w each year within fishing_years list
    map_years[[dfname]]@data$CATCH[is.na(map_years[[dfname]]@data$CATCH)] <- 0 # Fill NAs with 0s, otherwise any countries w NA just won't show up in the cartogram
    map_years[[dfname]]@data$CATCH[map_years[[dfname]]@data$CATCH < 1] <- 1 # Fill 0s with 1s so cartogram actually works 
    rm(dfname) # remove the floaters
    rm(year)
}

# ----------------
# 04 CARTOGRAM LOOP
# ----------------

# Create shapefiles directory - only need to do this once
#dir.create("Shapefiles")

# Now make the cartograms, fill the carto_maps dataframe with them, and save them as shapefiles! FYI this will take FOREVER. Each iteration takes ~1 minute; 50 iterations per map; 65 maps.
carto_maps <- list() # Empty list that will contain cartogram output. 

#fishtogram <- function(year) {
#  print(year)
#  dfname <- paste0("carto",year) # name of cartogram being made
#  map_year <- get(paste0("map", year), map_years) # Create 'map_year' and fill it with one SpatialPolygonsDataFrame #of a year of fishing/country data pulled from the list of spdf's 'map_years'
#  carto_maps[[dfname]] <<- cartogram(map_year, "CATCH", itermax=1) # USE ONE ITERATION FOR TESTING. This is the part #that takes forever. Create cartogram named 'dfname', chuck it into the carto_maps list
#  plot(carto_maps[[dfname]], main=dfname) # plot it
#  print(paste("Finished", dfname, "at", Sys.time())) # print time finished cartogram
#  writeOGR(obj = carto_maps[[dfname]], dsn = "Shapefiles", layer = dfname, driver = "ESRI Shapefile", overwrite_laye#r=TRUE) # Save shapefile, overwrite old ones if necessary, otherwise this forever code ABORTS when it was supposed #to be running OVERNIGHT -_-
#}

# Loop through given years and apply function fishtogram to those years (R is v slow at for loops.)
# lapply(seq(1975, 1977, 2), fishtogram)

# ***********************************
# PARALLELIZATION ATTEMPT
# ***********************************

# Parallelization
#if (!require(parallel)) {
#  install.packages("parallel", repos = "http://cran.stat.sfu.ca/")
#  require(parallel)
#}
#if (!require(doParallel)) {
#  install.packages("doParallel", repos = "http://cran.stat.sfu.ca/")
#  require(doParallel)
#}

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)

#clusterExport(cl, "fishtogram")
#clusterExport(cl, "year")
#clusterExport(cl, "dfname")
#clusterExport(cl, "map_year")
#clusterExport(cl, "map_years")
#clusterExport(cl, "carto_maps")
#clusterEvalQ(cl, library(cartogram))
#clusterEvalQ(cl, library(rgdal))
#clusterExport(cl, "writeOGR")
#clusterExport(cl, "plot")

#parLapply(cl, seq(1975, 1977, 2), fishtogram)

#stopCluster(cl)

# ----------------
# FFTW CARTOGRAMS
# ----------------

# THIS IS A HOT MESS ON WINDOWS BUT MAKES NICE CARTOGRAMS. MUCH easier if you're working on a Unix system.
# HOW TO GET FFTW/RCARTOGRAM TO WORK ON WINDOWS: (most recent) https://github.com/Geoff99/Rcartogram/blob/WindowsInstall/vignettes/README.WindowsInstall.Tutorial.Rmd 
# (but also helpful) https://stackoverflow.com/questions/31613119/installing-rcartogram-packages-error-message 

 # 01 FIRST, DOWNLOAD THE LATEST FFTW (Fast Fourier Transform) PACKAGE. The next two GitHub cartogram packages will NOT install without these binaries. http://www.fftw.org/download.html 
   # HOW TO INSTALL FFTW ON WINDOWS FOR DUMMIES: https://stackoverflow.com/questions/39675436/how-to-get-fftw-working-on-windows-for-dummies
 # 02 NEXT, install RTOOLS. Rcartogram (or any other R package that relies on C, for that matter) requires Rtools to work.
   #  

# Also install the R package "fftw": 
if (!require(fftw)) {
  install.packages("fftw", repos = "http://cran.stat.sfu.ca/")
  require(fftw) # fast Fourier transform
}
# Install & load devtools in order to use GitHub packages
if (!require(devtools)) {
  install.packages("devtools", repos = "http://cran.stat.sfu.ca/")
  require(devtools)
}
#install_github('omegahat/Rcartogram') # Actually, if on Windows, need to manually download & install (see above first tutorial link): https://github.com/omegahat/Rcartogram 
# MAKE SURE YOU ARE USING THE "WINDOWSINSTALL" BRANCH OF THIS GUY'S FORK!! 
# In git bash: cd to the cloned directory, then use: 'git checkout WindowsInstall'
# If you get an error saying something like "C:/Rtools/mingw_64/bin/gcc: not found", see my comment on this answer HERE: https://stackoverflow.com/questions/33103203/rtools-is-not-being-detected-from-rstudio/44035904#44035904 

# Wait for installation, and then:
install_github('chrisbrunsdon/getcartr', subdir='getcartr')

# Now load these bad boys that have been causing so much grief:
library(Rcartogram)
library(getcartr)

testcarto <- quick.carto(map_years[["map1956"]], map_years[["map1956"]]@data$CATCH, blur = 1)
plot(testcarto, main="carto1956")

fishtogram <- function(year) {
  print(year)
  dfname <- paste0("carto",year) # name of cartogram being made
  map_year <- get(paste0("map", year), map_years) # Create 'map_year' and fill it with one SpatialPolygonsDataFrame of a year of fishing/country data pulled from the list of spdf's 'map_years'
  carto_maps[[dfname]] <<- quick.carto(map_year, map_year@data$CATCH, blur = 1) # Create cartogram named 'dfname', chuck it into the carto_maps list
  plot(carto_maps[[dfname]], main=dfname) # plot it
  print(paste("Finished", dfname, "at", Sys.time())) # print time finished cartogram
  writeOGR(obj = carto_maps[[dfname]], dsn = "Shapefiles", layer = dfname, driver = "ESRI Shapefile", overwrite_layer=TRUE) # Save shapefile, overwrite old ones if necessary, otherwise this forever code ABORTS when it was supposed to be running OVERNIGHT -_-
}

# Testing 2 years
fishtogram(1956)
fishtogram(2014)

# NOW LOOP THROUGH THESE BAD BOYS!!
lapply(years, fishtogram)

# ----------------
# 05 PLOT
# ----------------

# Transform all from SpatialPolygonsDataFrame to ggplot-friendly dataframe
gg1950 <- tidy(carto1950) # This works
gg1950 <- merge(gg1950, carto1950@data, by="id") # Need to add index "id" column in original carto1950 data for this merge to work

p1950 <- ggplot() + 
  geom_polygon(data = subset(gg1950, id == 103), aes(
                #fill = id, 
                x = long,
                y = lat,
                group = group))
plot(p1950)


# ORIGINAL GGPLOT TESTING

# NEED TO GO BACK TO USING COUNTRIES DATASET 
# Doesn't split Chukchi peninsula

countries <- map_data("world")
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
  geom_polygon(data = countries, aes(
                                  #fill = Catch,
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
