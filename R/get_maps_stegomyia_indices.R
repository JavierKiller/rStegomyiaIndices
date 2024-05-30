#' get_maps_stegomyia_indices
#'
#' This function generates maps for three Stegomyia indices (Entomological Risk
#' Indices) of dataframe make to funtion
#' get_stegomyia_indices_by_type_of_study_and_geo_is.
#' This data frame containing information about sectors and their corresponding
#' indices.
#'
#' @param df the dataframe with information.
#' @param m1 is a shapefile"ejercicio_sectores_hermosillo.shp" with geographic
#'  information of hermosillo city for join dataframe with information of
#'  indices stegomyia
#' @param m0 is a shapefile "ejercicio_sectores_hermosillo2.shp" with limit of
#' sectors of hermosillo city. This shapefile use to fution with m1 and give
#' limits of the sectors and the hermosillo city
#'
#'   print(maps$HI)  # Print the map for the House Index
#'   print(maps$CI)  # Print the map for the Container Index
#'   print(maps$BI)  # Print the map for the Breteau Index

#' @return listmaps: A list of three ggplot objects representing the maps for different Stegomyia indices.
#'
#' @examples
#'
#' get_maps_stegomyia_indices(df,
#'  m1 =
#'  )
#'
#' get_maps_stegomyia_indices(df = "~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv" ,
#' m1 = "ejercicio_sectores_hermosillo.shp",
#' m0 = "ejercicio_sectores_hermosillo2.shp"
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

  #lista de colores
  colores_id <- c(
    "Optimo" = "blue",
    "Bueno" = "green",
    "Alarma" = "yellow",
    "Emergencia" = "red"
  )
  df_HI <- fortify(dw,
                 region = "ind__HI")
 #map HI
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
    coord_map() +
    labs(title = "Stegomyia House Index in Hermosillo") +
    scale_fill_manual(values = colores_id)

  df_CI <- fortify(dw,
                   region = "ind__CI")
 #map CI
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
    coord_map() +
    labs(title = "Stegomyia Container Index in Hermosillo") +
    scale_fill_manual(values = colores_id)

  df_BI <- fortify(dw,
                   region = "ind__BI")
 #map BI
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
    coord_map() +
    labs(title = "Stegomyia Breteau Index in Hermosillo") +
    scale_fill_manual(values = colores_id)
  #return(p_BI)
  listmaps<-list(p_HI, p_CI, p_BI)

print(listmaps)

}

# library(GISTools)
# library(ggplot2)
# library(Cairo)
# library(tidyverse)
# library(raster)
# library(rgeos)
# library(maptools)
# library(ggmap)
# library(ggplot2)
# library(sf)
