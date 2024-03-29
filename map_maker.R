## ----setup, include=FALSE---------------------------------
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


## ---------------------------------------------------------
basin_path <- "L:/DOW/BWAM Share/SMAS/data/archive/map_files"
# basin_path <- "L:/BWAM Share/SMAS/data/archive/map_files" #for keleigh

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


## ---------------------------------------------------------
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
bbox_new[3] <- bbox_new[3] + (0.4 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>% # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
# checking SSL


## ---------------------------------------------------------
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


## ---------------------------------------------------------

# read in the analysis data
sps_data <- read.csv("outputs/mean_subpop_basin_17_21_cycle.csv")
sps_data$Basin__ <- sps_data$Subpopulation
sps_data2<-sps_data %>% #take out housatonic and ramapo
  dplyr::filter(!Basin__ %in% c("15", "16", "All Sites"))
  

basin_map_append <- merge(basin_shp, sps_data2) # merge data by changing the subpopulation name


## ---------------------------------------------------------

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


## ---------------------------------------------------------

# read in the analysis data for first cycle
# sps_data2 <- read.csv("outputs/mean_subpop_basin_2008_2012_cycle.csv")
# sps_data2$Basin__ <- sps_data2$Subpopulation
# 
# basin_map_append2 <- merge(basin_shp, sps_data2) # merge data by changing the subpopulation name


## ---------------------------------------------------------

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


## ----create-tables-for-writeup----------------------------

sps_data_short <- sps_data |> 
  dplyr::select(Subpopulation,
                Estimate,
                MarginofError,
                nResp) %>%
  dplyr::mutate(Estimate = sprintf("%.1f",
                                   round(Estimate, 1)),
                MarginofError = sprintf("%.1f",
                                        round(MarginofError, 1))                  ) |>
    tidyr::unite(
      col = "average_ci",
      c("Estimate", "MarginofError"),
      sep = " \u00B1 "
    ) |>
  dplyr::mutate(average_ci = dplyr::if_else(Subpopulation %in% c("15", "16"),                                                       "NA",                                                        average_ci)) |> 
  dplyr::arrange(suppressWarnings(as.numeric(Subpopulation))) |> 
  dplyr::select(
    "basin_num" = Subpopulation,
    "Sample Size" = nResp,
    "Estimated Average BAP Score" = average_ci
  )  
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

basins <- basin_shp@data %>%
  dplyr::select(Basin, Basin__) %>%
  dplyr::rename(Name = Basin) %>%
  mutate(
    Name = case_when(
      grepl("Ontario", Name) ~ "Lake Ontario",
      grepl("Niagara", Name) ~ "Lake Erie-Niagara River",
      grepl("Oswego", Name) ~ "Oswego River",
      TRUE ~ Name
    ),
    basin_num = as.character(Basin__)
  ) %>%
  bind_rows(data.frame(Name = "Statewide",
                       basin_num = NA_character_)) |>
  distinct() |>
  dplyr::select(-Basin__)
sps_data_final <- left_join(sps_data_short, basins,
                            by = "basin_num")  |>
  arrange(as.numeric(basin_num)) |>
  mutate(
    Name = if_else(basin_num %in% "All Sites",
                   "State-Wide",
                   Name),
    basin_num = if_else(basin_num %in% "All Sites",
                        "",
                        basin_num)
  ) |>
  relocate("Basin Number" = basin_num,
           "Major Drainage Basin" = Name)



(tab <- table.f(sps_data_final, 2, ncol(sps_data_final)))


