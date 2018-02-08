setwd('C:/Users/spopov/Documents/GIS/Deng/400m-cartograms')

fishing_data <- read.csv('FAOSAU_400m_country_gis.csv')
colnames(fishing_data)[1] <- "NAME"
years <- unique(fishing_data$Year)

library(ggplot2)
library(dplyr)
library(rgdal)
library(broom)
library(rmapshaper)

carto1950 <- readOGR('Shapefiles/carto1950.shp')
carto1950@data$id <- seq.int(nrow(carto1950@data)) - 1 # Create ID column based on index of dataframe; this is what becomes the "id" column when you tidy the dataset for ggplot. We'll then use this id column to join the catch data from our original dataset to the tidied ggplot dataset.

carto1950simple <- ms_simplify(carto1950, keep=0.31, method="vis")

ggdata <- tidy(carto1950simple)
ggdata <- merge(x = ggdata, y = carto1950@data[,c("NAME", "id")], by="id", all.x=TRUE) # Join original country & catch data to tidied dataset
ggdata <- merge(x = ggdata, y = fishing_data[,c("NAME", "Catch")], by="NAME", all.x = TRUE) # This is messy, but merging with original dataset bc changed 0s to 1s in the cartogram plots to get them to work. Need 0s for plotting color scale. 
ggdata$Catch[is.na(ggdata$Catch)] <- 0 # replace NAs with 0
ggdata <- arrange(ggdata, order) # tidy up again for fussy ggplot

# Set bins
bins <- c(0, 1, 5000, 20000, 50000, 100000, 200000, 300000, 407719)
ggdata$bins <- cut(
  ggdata$Catch, 
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
#col.pal <- rev(col.pal) # reverse color order
col.pal <- c("#b7b7b7", col.pal) # Add grey to palette for 0 catch legend items
p1950 <- p1950 +
  scale_fill_manual(
    values = col.pal,
    name = "Catch (tonnes)"
    )
plot(p1950)

