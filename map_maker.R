## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


# checkpoint::checkpoint(snapshot_date = "2022-11-17")

library(dplyr)
# for loading our data
library(raster)
library(readr)
library(readxl)
library(sf)
# for datasets
library(maps)
library(spData)
# for plotting
library(flextable)
library(grid)
library(tmap)
library(viridis)


## ------------------------------------------------------------------------------
basin_path <- "L:/DOW/BWAM Share/SMAS/data/map_files"
basin_path <- "L:/BWAM Share/SMAS/data/map_files" #for keleigh

basin <- rgdal::readOGR(
  dsn = basin_path,
  layer = "basin_big",
  verbose = FALSE
)

# change coords to web mercator for the map
basin_shp <- sp::spTransform(
  basin,
  sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
)


## ------------------------------------------------------------------------------
states <- sf::st_as_sf(map("state",
                           plot = FALSE,
                           fill = TRUE))
nys <- states %>%
  filter(ID == "new york")

# get bounding for the outline this is for the inset map, to make it have a border
outline.df <- ggplot2::fortify(nys)
nybox <- sf::st_as_sfc(sf::st_bbox(outline.df))

# make some bbox magic to give it a little border
bbox_new <- st_bbox(nybox) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>% # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
# checking SSL


## ------------------------------------------------------------------------------
# basin_map <- tm_basemap(c(
#   StreetMap = "OpenStreetMap",
#   TopoMap = "OpenTopoMap"
# )) +
#   # tmap_options(check.and.fix = TRUE) +
#   # tmap::tm_shape(nys, bbox = bbox_new) +
#   # tmap::tm_fill() +
#   tm_shape(basin_shp,
#     bbox = bbox_new
#   ) +
#   tm_fill(col = "Basin__", alpha = .9, title = "Basin ID") +
#   tm_borders() +
#   tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
#   tm_compass(position = c("left", "top"), size = 2) +
#   tm_layout(
#     title = "Statewide Probabalistic Analysis",
#     legend.position = c("right", "top")
#   ) +
#   tm_credits("Data source:2016-2021 ",
#     fontface = "italic",
#     align = "right"
#   ) +
#   tm_credits("Author: SMAS ",
#     fontface = "bold",
#     align = "right"
#   )
# basin_map


## ------------------------------------------------------------------------------

# read in the analysis data
sps_data <- read.csv("outputs/mean_subpop_basin_17_21_cycle.csv")
sps_data$Basin__ <- sps_data$Subpopulation
sps_data2<-sps_data %>% #take out housatonic and ramapo
  dplyr::filter(Basin__!=15)
  

basin_map_append <- merge(basin_shp, sps_data2) # merge data by changing the subpopulation name


## ------------------------------------------------------------------------------

# viridis magma, specify 0-10 range in the tm_fill; just do the last cycle for the analysis-look this up to see how we do trend analysis-just do last 5

basin_map2 <- tm_basemap(c(
  StreetMap = "OpenStreetMap",
  TopoMap = "OpenTopoMap"
)) +
  # tmap::tm_shape(nys,
  #                bbox = bbox_new) +
  # tmap::tm_fill() +
  tm_shape(basin_map_append,
    bbox = bbox_new
  ) +
  tm_fill(
    col = "Estimate", alpha = .9,
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    title = "Estimated BAP", palette = "-viridis"
  ) +
  tm_borders() +
  tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
  tm_compass(position = c("left", "top"), size = 2) +
  tm_layout(title = "Statewide Probabalistic Analysis",
            legend.position = c("right", "top")) +
  tm_credits("Data source:2017-2021 ",
    fontface = "italic",
    align = "right"
  ) +
  tm_credits("Author: SMAS ",
    fontface = "bold",
    align = "right"
  )+tm_text("Basin__", size=0.5)
basin_map2


## ------------------------------------------------------------------------------

# read in the analysis data for first cycle
# sps_data2 <- read.csv("outputs/mean_subpop_basin_2008_2012_cycle.csv")
# sps_data2$Basin__ <- sps_data2$Subpopulation
# 
# basin_map_append2 <- merge(basin_shp, sps_data2) # merge data by changing the subpopulation name


## ------------------------------------------------------------------------------

# viridis magma, specify 0-10 range in the tm_fill; just do the last cycle for the analysis-look this up to see how we do trend analysis-just do last 5

# basin_map_first <- tm_basemap(c(
#   StreetMap = "OpenStreetMap",
#   TopoMap = "OpenTopoMap"
# )) +
#   # tmap::tm_shape(nys,
#   #                bbox = bbox_new) +
#   # tmap::tm_fill() +
#   tm_shape(basin_map_append2,
#     bbox = bbox_new
#   ) +
#   tm_fill(
#     col = "Estimate", alpha = .9,
#     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     title = "Estimated BAP", palette = "-viridis"
#   ) +
#   tm_borders() +
#   tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
#   tm_compass(position = c("left", "top"), size = 2) +
#   tm_layout(title = "Statewide Probabalistic Analysis",
#             legend.position = c("right", "top")) +
#   tm_credits("Data source:2008-2012 ",
#     fontface = "italic",
#     align = "right"
#   ) +
#   tm_credits("Author: SMAS ",
#     fontface = "bold",
#     align = "right"
#   )
# 
# basin_map_first


## ----create-tables-for-writeup-------------------------------------------------

sps_data_short <- sps_data %>%
  dplyr::select(Subpopulation, Estimate, StdError,nResp) %>%
  dplyr::rename(
    Basin = Subpopulation,
    n = nResp,
    "Estimated BAP Score" = Estimate,
    "Standard Error" = StdError
  ) %>%
  mutate_if(is.numeric,
    round,
    digits = 2
  ) # round the digits in the table to something more palatable
# table function
table.f <- function(df, x, y) {
  library(flextable)
  tl <- flextable(df) %>%
    font(i = NULL, j = NULL, fontname = "Arial", part = "all") %>%
    theme_zebra()
  tl <- fontsize(tl, size = 8, part = "all")
  tl <- autofit(tl)
  tl <- set_table_properties(tl, layout = "autofit")
  tl <- align(tl, i = NULL, j = (x:y), align = "center", part = "all")
  tl
}

#merge with basin name
basins<-basin_shp@data
basins<-basins %>% 
  dplyr::select(Basin,Basin__) %>% 
  dplyr::rename(Name=Basin) %>% 
  mutate(Name=case_when(Name=="Lake Ontario1"~"Lake Ontario and Tribs",
                        Name=="Lake Ontario3"~"Lake Ontario and Tribs",
                        Name=="Lake Ontario4"~"Lake Ontario and Tribs",
                        Name=="Lake Ontario Tributaries"~"Lake Ontario and Tribs",
                        TRUE~Name)) %>% 
  distinct()
sps_data_short<-merge(sps_data_short,basins,
                      by.x="Basin",
                      by.y="Basin__")


tab <- table.f(sps_data_short, 2, ncol(sps_data_short))
tab


