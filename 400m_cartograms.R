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
dir.create("Shapefiles")

# Now make the cartograms, fill the carto_maps dataframe with them, and save them as shapefiles! 
carto_maps <- list() # Empty list that will contain cartogram output. 

# ----------------
# FFTW CARTOGRAMS
# ----------------

# THIS IS A HOT MESS ON WINDOWS BUT MAKES NICE CARTOGRAMS. MUCH easier if you're working on a Unix system.
# HOW TO GET FFTW/RCARTOGRAM TO WORK ON WINDOWS: (most recent) https://github.com/Geoff99/Rcartogram/blob/WindowsInstall/vignettes/README.WindowsInstall.Tutorial.Rmd 
# (but also helpful, I left some comments on the answer if you ever run into any of those issues) https://stackoverflow.com/questions/31613119/installing-rcartogram-packages-error-message 

 # 01 FIRST, DOWNLOAD THE LATEST FFTW (Fast Fourier Transform) PACKAGE (both 32 bit & 64 bit). The next two GitHub cartogram packages will NOT install without these binaries. http://www.fftw.org/download.html 
 # 02 NEXT, install RTOOLS. Rcartogram (or any other R package that relies on C, for that matter) requires Rtools to work. https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows 
 # 03. NEXT, go to the tutorial in the very first link to get this nightmare off the ground and running. Once you do, it will be so worth it. 

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

fishtogram <- function(year) {
  print(year)
  dfname <- paste0("carto",year) # name of cartogram being made
  map_year <- get(paste0("map", year), map_years) # Create 'map_year' and fill it with one SpatialPolygonsDataFrame of a year of fishing/country data pulled from the list of spdf's 'map_years'
  carto_maps[[dfname]] <<- quick.carto(map_year, map_year@data$CATCH, blur = 1) # Create cartogram named 'dfname', chuck it into the carto_maps list
  rownames(carto_maps[[dfname]]@data) <<- 1:nrow(carto_maps[[dfname]]@data) # Reset row names/numbers index
  carto_maps[[dfname]]@data$id <<- seq.int(nrow(carto_maps[[dfname]]@data)) # Create ID column based on index of dataframe; this is what becomes the "id" column when you tidy the dataset for ggplot. We'll then use this id column to join the catch data from our original dataset to the tidied ggplot dataset later.
  plot(carto_maps[[dfname]], main=dfname) # plot it
  print(paste("Finished", dfname, "at", Sys.time())) # print time finished cartogram
  writeOGR(obj = carto_maps[[dfname]], dsn = "Shapefiles", layer = dfname, driver = "ESRI Shapefile", overwrite_layer=TRUE) # Save shapefile, overwrite old ones if necessary
}

# Testing 2 years
fishtogram(1950)
fishtogram(2014)

# NOW LOOP THROUGH THESE BAD BOYS!!
lapply(years, fishtogram)

# ----------------
# 05 PLOT
# ----------------

carto1950 <- carto_maps[["carto1950"]] # Pull one map for testing

ggdata <- tidy(carto_maps[["carto1950"]])
ggdata <- merge(x = ggdata, y = carto1950@data[,c("NAME","CATCH","id")], by="id", all.x=TRUE) # Join original country & catch data to tidied dataset by id column
#ggdata <- merge(x = ggdata, y = fishing_data[,c("NAME", "CATCH")], by="NAME", all.x = TRUE) # This is messy, but merging with original dataset bc changed 0s to 1s in the cartogram plots to get them to work. Need 0s for plotting color scale. 
ggdata$CATCH[is.na(ggdata$CATCH)] <- 0 # replace NAs with 0

ggdata <- arrange(ggdata, order) # tidy up again for fussy ggplot

# Set bins
bins <- c(0, 2, 5000, 20000, 50000, 100000, 200000, 300000, 407719) # Anything with 1 catch in the dataset is actually binned as zero bc I changed all zeros to 1s for cartogram calculation
ggdata$bins <- cut(
  ggdata$CATCH, 
  breaks = bins, 
  labels = c("0", "1-5000", "5001 - 20 000", "20 001 - 50 000", "50 001 - 100 000", "100 001 - 200 000", "200 001 - 300 001", "300 001 - 407719"),
  right = FALSE
)

# Basic plot
p1950 <- ggplot(
  # set mappings for each layer
  data = ggdata, 
  aes(
    x = long, 
    y = lat, 
    group = group
  )
) +
  # cartogram
  geom_polygon(
    aes (fill = bins)
  ) +
  # cartogram outlines
  geom_path(
    color = "#5e5e5e", #2b2b2b
    size = 0.5
  ) +
  # constrain proportions
  coord_fixed()
#plot(p1950)

# Now make the theme nice
library(showtext)
font_add_google("Karla", "karla") # Add nice google font
showtext_auto() # Tell R to use showtext to render google font
# Nice fonts don't work >:(

#Manually downloaded Karla
windowsFonts(Karla=windowsFont("Karla"))

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Karla", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

p1950 <- p1950 + 
  theme_map() +
  labs(
    x = NULL,
    y = NULL,
    title = "1950",
    caption = "FAO + SAU data"
  )
#plot(p1950)

# Better color scale
library(RColorBrewer)
col.pal <- brewer.pal(7, "Spectral") # Add nice Yellow-green-blue palette for colored legend items
col.pal <- rev(col.pal) # reverse color order
col.pal <- c("#b7b7b7", col.pal) # Add grey to palette for 0 catch legend items
p1950 <- p1950 +
  scale_fill_manual(
    values = col.pal,
    name = "Catch (tonnes)",
    drop = FALSE
  )
plot(p1950)



