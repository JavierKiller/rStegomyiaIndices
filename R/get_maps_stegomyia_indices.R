#get_maps_stegomyia_indices
#library(GISTools)
# library(ggplot2)
# library(tidyverse)
# library(raster)
# library(rgeos)
# library(maptools)
# library(ggmap)
# library(sf)
# library(Cairo)
# setwd("C:/Users/Javier Edgar Verdugo/Documents/CursoQR/Package1/rStegomyiaIndices/R")

#get_maps_stegomyia_indices
#load to data table
#### agregar documentacion y tests
dft <- read.csv("~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv")
df <- dft

#load to map
w1 <- st_read("~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo.shp")
#loda original map
d0 <- readShapePoly("~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo2.shp")
#w1
#w2
#d0

#function
get_maps_stegomyia_indices <- function(
    df,
    m1 = w1,
    m0 = d0
    ){
  w2 <- st_transform(m1,
                   4326)
  #rename of variable
  w2 <- w2 %>%
    rename(Sector = SECCION)
  #join to data table
  w2_ <- w2 %>%
    left_join(df,
              by = "Sector")
  #w2_
  #save map with data and correct format
  st_write(w2_,
           dsn = "~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo_transformado.shp",
           append = F)
  #load to new map
  dw=readShapePoly("~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo_transformado.shp")
  # plot(dw)
  # names(dw)
  # summary(dw)
  df_HI <- fortify(dw,
                 region = "ind__HI")
  #df_HI
  p_HI <- ggplot() +
    geom_polygon(data = d0,
                 aes(x = long,
                     y = lat,
                     group = group),
                 color = "black",
                 size = 0.5,
                 alpha = 0) +
    geom_polygon(data = df_HI,
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = id),
                 color = "black",
                 size = 0.5) +
    coord_map()
  return(p_HI)
  #tail(df_HI)

  df_CI <- fortify(dw,
                   region = "ind__CI")
  #df_CI
  p_CI <- ggplot() +
    geom_polygon(data = d0,
                 aes(x = long,
                     y = lat,
                     group = group),
                 color = "black",
                 size = 0.5,
                 alpha = 0) +
    geom_polygon(data = df_CI,
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = id),
                 color = "black",
                 size = 0.5) +
    coord_map()
  return(p_CI)
  #tail(df_CI)

  df_BI <- fortify(dw,
                   region = "ind__BI")
  #df_BI

  p_BI <- ggplot() +
    geom_polygon(data = d0,
                 aes(x = long,
                     y = lat,
                     group = group),
                 color = "black",
                 size = 0.5,
                 alpha = 0) +
    geom_polygon(data = df_BI,
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = id),
                 color = "black",
                 size = 0.5) +
    coord_map()
  return(p_BI)
  #tail(df_BI)
}
