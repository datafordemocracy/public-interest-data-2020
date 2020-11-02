######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate map by census tract characteristics
# 1. Load libraries, set color palette
# 2. Pull Census Tracts, School Districts
# 3. Set map parameters
# 4. Generate base maps
# Updated: October 2020 (Hannah)
######################################################################################

# ..........................................................................................
# 1. Load libraries  ----

# load packages 
library(sf)
library(ggmap) 
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with
# ggplot2. The R Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
library(RColorBrewer)
library(tidyverse)
library(rgeos)
library(tigris)

# set color palette
pidlpal <- c("#91c4f2","#8e8dbe","#a66b92","#92d5e6","#d9f0ff", "#c1f7dc")


# ..........................................................................................
# 2. Pull Census Tracts, School Districts  ----

## Census Tracts
#set sf option
options(tigris_class = "sf")
cville <- tracts("VA", county = "540", cb = T)

# adding sorted delineation from section III of report
cville <- cville %>%
  mutate(perc_wt = fct_collapse(cville$NAME, 
                                ">75" = c("9", "10", "4.02", "7"),
                                "50-75" = c("3.02", "5.02", "2.01", 
                                            "2.02", "6", "8"),
                                "<50" = c("4.01", "5.01")),
         perc_pov = fct_collapse(cville$NAME,
                                 ">20" = c("2.02", "6", "4.01", 
                                           "2.01", "5.01"),
                                 "10-20" = c("7", "10", "8", 
                                             "4.02"),
                                 "<10" = c("9", "3.02", "5.02"))) %>%
  mutate(perc_wt = fct_relevel(perc_wt, "<50", "50-75", ">75")) %>%
  mutate(perc_pov = fct_relevel(perc_pov, ">20", "10-20", "<10"))

## School Districts
# setwd("~/Desktop/PIDL /final/elementary_school_zone_area")

# read in school zone data pulled from:
# https://opendata.charlottesville.org/datasets/elementary-school-zone-area
sch <- st_read("Elementary_School_Zone_Area-shp/Elementary_School_Zone_Area.shp")


# ..........................................................................................
# 3. Set map parameters ----
# followed spatial mapping tutorial from 
# https://cengel.github.io/R-spatial/mapping.html

# Convert Census sf to 4326
cville_4326 <- st_transform(cville, 4326)

locals <- matrix(1:24, nrow = 12, ncol = 2)
for(i in 1:12){
  d <- as.data.frame(cville[[10]][[i]][[1]])
  long <- mean(d$X1)
  lat <- mean(d$X2)
  locals[i,1] <- long
  locals[i,2] <- lat
}
locals <- as.data.frame(locals)
tracts <- as.data.frame(cville$NAME)

tract_lbs <- cbind(tracts, locals)
names(tract_lbs) <- c("tract", "long", "lat")

# find values for stamen map, by pulling max and min lat long values from the school
# district shape files
sch_ll <- st_transform(sch, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# st_bbox(sch_ll)
# xmin      ymin      xmax      ymax
# -78.52377  38.00967 -78.44636  38.07053

# set boundaries of stamen map 
cville_bb <- c(left = -78.528,
               bottom = 38.007,
               right = -78.442,
               top = 38.072)

# create background map tile
test_map_cville <- get_stamenmap(bbox = cville_bb, zoom = 14, maptype = "toner-lite")
ggmap(test_map_cville)


# ..........................................................................................
# 4. Generate base maps ----
# try plotting census tracts + filling by perc_pov and perc_wt
ggmap(test_map_cville) +
  geom_sf(data = cville_4326, inherit.aes = FALSE, color = "#37323E", size = 1, aes(fill = perc_pov), alpha = 0.7) +
  geom_text(data = tract_lbs, aes(label = paste("Census Tract", tract), x = long, y=lat), color = "#37323E") +
  scale_fill_manual(name="Percent of families living \nat or below the federal \npoverty line", 
                    breaks=c(">20", "10-20", "<10"),
                    labels=c("Greater than 20%", "10% to 20%", "Less than 10%"), values=pidlpal) +
  scale_color_manual(values = pidlpal) +
  theme_void() +
  labs(title = "Charlottesville Census Tracts by Poverty and Racial Demographic Distinction",
       subtitle = "Census tracts are split into three categories based on race,and three categories\nbased on percent of families living at or below the poverty line\n",
       caption = "Data Sources from the Charlottesville Department of Child Protective Services and the US Census") + 
  coord_sf(crs = st_crs(4326))

ggmap(test_map_cville) +
  geom_sf(data = cville_4326, inherit.aes = FALSE, color = "#37323E", size = 1, aes(fill = perc_wt), alpha = 0.7) +
  geom_text(data = tract_lbs, aes(label = paste("Census Tract", tract), x = long, y=lat), color = "#37323E") +
  scale_fill_manual(name="Proportion of non-Hispanic \nWhite Residents",
                    breaks=c("<50", "50-75", ">75"),
                    labels=c("Less than 50%", "50% to 75%", "Greater than 75%"), values=pidlpal) +
  scale_color_manual(values = pidlpal) +
  theme_void() +
  coord_sf(crs = st_crs(4326))

