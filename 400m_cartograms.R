# 400m Cartograms
# Deep water fisheries manuscript

# 01 This Datacarpentry workshop is overall super helpful for any R GIS: http://www.datacarpentry.org/R-spatial-raster-vector-lesson/ 
# 02 This GitHub.io page goes through basic vector/shapefile plotting: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# 03 Plotting cartograms using R: http://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html
# 04 Africa cartogram animation: https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/ 

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
if (!require(rmapshaper)) {
  install.packages("rmapshaper", repos = "http://cran.stat.sfu.ca/")
  require(rmapshaper)
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

data("wrld_simpl") # Simple world dataset from maptools
world <- spTransform(wrld_simpl, CRS("+proj=eqc +ellps=WGS84 +datum=WGS84 +no_defs")) # Add EPSG 4326 projection
world <- world[world$NAME != "Antarctica", c("ISO3", "NAME", "REGION")] # World GIS SpatialPolygonsDataFrame (spdf), minus Antarctica, minus irrelevant columns (hopefully subregion is actually irrelevant loooool)
world <- ms_simplify(world, keep = 0.3, keep_shapes=TRUE) # Simplify geometries using rmapshaper. Might need to add 'keep_shapes=TRUE' to prevent deletion of small features if this creates merging issues later down the road.
rownames(world@data) <- world@data$ISO3
rm(wrld_simpl)

#world2 <- spTransform(world, CRS("+proj=eqc +ellps=WGS84 +datum=WGS84 +lon_0=10 +no_defs")) # maybe eventually shift central meridian over to +10, so that Chukchi peninsula is not chopped off Russia. 
#<object>@proj4string # Check CRS of a Spatial*DataFrame object.

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
    map_years[[dfname]]@data$YEAR[is.na(map_years[[dfname]]@data$YEAR)] <- year # fill NA years with the current year
    rm(dfname) # remove the floaters
    rm(year)
}


# *****************
# FFTW INSTALLATION
# *****************

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
# install_github('chrisbrunsdon/getcartr', subdir='getcartr')

# Now load these bad boys that have been causing so much grief:
library(Rcartogram)
library(getcartr)


# ----------------
# 04 CARTOGRAM LOOP
# ----------------

# Create shapefiles directory - only need to do this once
dir.create("Shapefiles")

# Now make the cartograms, fill the carto_maps dataframe with them, and save them as shapefiles! 
carto_maps <- list() # Empty list that will contain cartogram output.

fishtogram <- function(year) {
  print(year)
  dfname <- paste0("carto",year) # name of cartogram being made
  map_year <- get(paste0("map", year), map_years) # Create 'map_year' and fill it with one SpatialPolygonsDataFrame of a year of fishing/country data pulled from the list of spdf's 'map_years'
  carto_maps[[dfname]] <<- quick.carto(map_year, map_year@data$CATCH, blur = 1) # Create cartogram named 'dfname', chuck it into the carto_maps list
  #rownames(carto_maps[[dfname]]@data) <<- 1:nrow(carto_maps[[dfname]]@data) # Reset row names/numbers index
  #carto_maps[[dfname]]@data$id <<- seq.int(nrow(carto_maps[[dfname]]@data)) # Create ID column based on index of dataframe; this is what becomes the "id" column when you tidy the dataset for ggplot. We'll then use this id column to join the catch data from our original dataset to the tidied ggplot dataset later.
  #carto_maps[[dfname]]@data$id <<- rownames(carto_maps[[dfname]]@data)
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
# 05 PLOT & ANIMATE
# ----------------

# Create smoothly animated maps between each year.
# You will need the tweenr library and gganimate library. 
# For gganimate to run correctly, you need to manually install ImageMagick: 
# https://www.imagemagick.org/script/index.php 
# NOTE on Windows bc Windows just REALLY doesn't want to cooperate with this you need to tick the "Install legacy components" checkbox when doing installation otherwise gganimate won't work. 

# Or, you can install from within R:
#library(installr); install.ImageMagick(URL = "https://www.imagemagick.org/script/download.php")

if (!require(tweenr)) {
  install.packages("tweenr", repos = "http://cran.stat.sfu.ca/")
  require(tweenr)
}

# If gganimate doens't work after installing ImageMagick & gganimate, you may need to restart R.  
# install_github("dgrtwo/gganimate")
library(gganimate)

# Spatial*DataFrames need to be "tidied" into regular dataframes using the broom library to be ggplot-friendly.
tidy_cartos <- list() # Empty list to contain all tidied cartograms
# Set bins for each cartogram (for later plotting)
bins <- c(0, 2, 5000, 20000, 50000, 100000, 200000, 300000, 407719) # Anything with 1 catch in the dataset is actually binned as zero bc I changed all zeros to 1s for cartogram calculation
# Tidy up each cartogram and put them all into tidy_cartos list

tidygram <- function(year) {
  ggdata <- get(paste0("carto", year), carto_maps) # Pull current year cartogram into ggdata
  ggdata <- tidy(ggdata, region = "ISO3") # Tidy; tell it to use ISO3 as unique id
  names(ggdata)[names(ggdata) == 'id'] <- "ISO3" # rename id column to ISO3
  
  # THIS IS IN HERE FOR THE COMMIT RECORD:
  # For whateverass reason the id column is the ISO code so rename it to that LATER EDIT: THIS PREVIOUSLY WORKED AND NOW IT'S NOT WTF?? uuguhhhghh
  # UPDATE: the root of this stupid fucking issue is the simplification algorithm at the top. even if I manually rename the rownames of the simplified world map, tidy for whatever(ass) reason uses the ORIGINAL rownames that ms_simplify spits out for the geometry. So even if I change the rownames of the simplified map, even if I change the rownames of the cartogram, tidy will ALWAYS create a stupid gotdamn column called 'id' that is filled with whatever rownames were first spat out by ms_simplify. 
  # THIS IS FIXED BY SPECIFYING THE REGION IN THE TIDY FUNCTION. in this case, defining the region by 'region = "ISO3"'. 
  ggdata <- merge(x = ggdata, y = carto_maps[[paste0("carto",year)]]@data[,c("NAME","YEAR","CATCH", "ISO3")], by="ISO3", all.x=TRUE) # Join original country, year & catch data to tidied dataset by id column
  ggdata$CATCH[is.na(ggdata$CATCH)] <- 0 # replace NA catches with 0
  ggdata$YEAR[is.na(ggdata$YEAR)] <- year # replace NA years with current year
  ggdata$bins <- cut(
    ggdata$CATCH, 
    breaks = bins, 
    labels = c("0", "1-5000", "5001 - 20 000", "20 001 - 50 000", "50 001 - 100 000", "100 001 - 200 000", "200 001 - 300 001", "300 001 - 407719"),
    right = FALSE
  )
  dfname <- paste0("tidy", year)
  print(dfname)
  tidy_cartos[[dfname]] <<- ggdata # Add to list
  rm(dfname)
  rm(year)
  rm(ggdata)
}

# loop through
lapply(years, tidygram)

# Merge every single tidied cartogram in this list into one giant dataframe:
all_maps <- dplyr::bind_rows(tidy_cartos)

# Test animation on subset 
sixties <- all_maps[1959 < all_maps$YEAR & all_maps$YEAR <1970, ]
sixties$coords <- paste(sixties$long,sixties$lat) # Create coordinates column for each unique coordinate. We're animating between coordinates per year. 
sixties$ease <- "quadratic-in-out"
# Tween this subset (totally does not work)
tw_sixties <- tween_elements(data = sixties, time = 'YEAR', group = 'coords', ease ='ease', nframes=15)

# Make nice ggplot theme
if (!require(showtext)) {
  install.packages("showtext", repos = "http://cran.stat.sfu.ca/")
  require(showtext)
}
font_add_google("Karla", "karla") # Add nice google font
showtext_auto() # Tell R to use showtext to render google font

# If windows is being crap, then:
# Manually downloaded Karla https://fonts.google.com/specimen/Karla 
windowsFonts(Karla=windowsFont("Karla"))

# **********************
# DISCRETE COLOR SCALE
# **********************

# Better color scale
library(RColorBrewer)
col.pal <- brewer.pal(7, "Spectral") # Add nice Yellow-green-blue palette for colored legend items
col.pal <- rev(col.pal) # reverse color order
col.pal <- c("#b7b7b7", col.pal) # Add grey to palette for 0 catch legend items

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

# Full plot, DISCRETE COLOR SCALE
p <- ggplot(
  # set mappings for each layer
  #data = tidy_cartos[["tidy1990"]][tidy_cartos[["tidy1990"]]$NAME == "Cyprus",],
  #data = tidy_cartos[["tidy1990"]][tidy_cartos[["tidy1990"]]$bins == "1-5000",],
  #data = tidy_cartos[["tidy1990"]],
  #data = map_years[["map1990"]],
  #data = sixties,
  data = all_maps,
  aes(
    x = long, 
    y = lat, 
    frame = YEAR,
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
  # country labels
  #geom_text(aes(label = NAME)) +
  # constrain proportions
  coord_fixed() +
  # add nice theme
  theme_map() +
  # color scale
  scale_fill_manual(
    values = col.pal,
    name = "Catch (tonnes)",
    drop = FALSE
  ) +
  # labels
  labs(
    x = NULL,
    y = NULL,
    title = "FAO + SAU catch in",
    caption = expression(paste("Victorero ", italic("et al."), " 2018"))
  )

plot(p)

# Will take a minute or two
gganimate(p,  interval=0.2)

#"discrete-FAO-SAU-animation.gif",



# ****************
# ORIGINAL GGPLOT TESTING II BELOW
# ****************

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



