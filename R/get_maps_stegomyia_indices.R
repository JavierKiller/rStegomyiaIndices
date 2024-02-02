#' get_maps_stegomyia_indices
#'
#'This function generates maps for three Stegomyia indices (Entomological Risk Indices) of dataframe make to funtion get_stegomyia_indices_by_type_of_study_and_geo_is.
#' This data frame containing information about sectors and their corresponding indices.
#'
#' @param df the dataframe with information.
#' @param m1 ~/CursoQR/Package1/rStegomyiaIndices/data-raw/table_indices_sector_in_date.csv
#' @param m0 "~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo_transformado.shp"
#'
#'   print(maps$HI)  # Print the map for the House Index
#'   print(maps$CI)  # Print the map for the Container Index
#'   print(maps$BI)  # Print the map for the Breteau Index

#' @return A list of three ggplot objects representing the maps for different Stegomyia indices.
#'
#' @examples
#' get_sector_with_star_date_by_type_study(df,
#' m1 = "/data-raw/example_table_indices_sector_in_date.csv",
#' ).
#'

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



#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type and geographic variable using the
#' following
#'    formulas:
#'       - Container Index(CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#' Compare the stegomyia indices in sector in map of location
#'
#'
#' param df the dataframe with information.
#' param m1 ~/CursoQR/Package1/rStegomyiaIndices/data-raw/table_indices_sector_in_date.csv
#' param m0 "~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo_transformado.shp"
#'
#' return
#'
#' The dataframe with stegomyia indices of the selection of type of study and
#' geographic variable.
#'
#' #export
#'
#' examples
#' get_sector_with_star_date_by_type_study(df,
#' pathout = "/data-raw/example_table_indices_sector_in_date.csv",
#' ).
#'
#'
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
#dft <- read.csv("~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv")
#df <- dft

#load to map
#w1 <- st_read("~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo.shp")
#loda original map
#d0 <- readShapePoly("~/CursoQR/Package1/rStegomyiaIndices/data-raw/maps/ejercicio_sectores_hermosillo2.shp")
#w1
#w2
#d0

#function
