#'  clean raw data from a entomology study
#'
#' @description
#' data.frame chance type data of variable. previously  load with function
#' "Load_raw_data" file .txt  of entomology study from platform
#' "Vigilancia Entomológica y Control Integral del Vector", and make a file
#' .csv of data.frame
#'
#' chose the variables
#' that use to calculate entomological risk indices. Fix variable labels
#' Eliminate spaces in the column names and replace them with underscores _.
#' A short description...mensionar la plataforma
#' data.fram from load_raw_data
#' mensionar todos los pasos
#'
#' @param `df` a data.frame with raw data path
#' @param `path_out` path for that cleaned data
#' @param `col_name` select variables
#'
#' @return A .csv file with selected and cleaned data
#' @export
#'
#' @examples
#'
#' path <- "./data-raw/estudio_entomologico.txt"
#'
#' df <- load_raw_data(df)
#'
require(tidyverse)
require(lazyeval)
require(lubridate)
require(ggplot2)
require(rlang)
require(testthat)
require(dplyr)
clean_raw_data <- function(
    df,
    colt= list(
      Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
      Clave_Jurisdiccion = "f",
      Jurisdiccion = "f",
      Clave_Localidad = "f",
      Localidad = "f",
      Sector = "f",
      Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
      Semana_Epidemiologica = "f",
      Casas_Revisadas = "d",
      Casas_Positivas = "d",
      Total_de_Recipientes_con_Agua = "d",
      Total_de_Recipientes_Positivos = "d"
    ),
    path_out = "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices/data-raw/qr.csv"
){

  write_csv(df, path_out)

  df_aux <- read_csv(
    path_out,
    col_types = colt,
  )
  df <- df_aux

  return(df)
}

