library(tidyverse)
library(lazyeval)
library(lubridate)
library(ggplot2)
library(rlang)
library(testthat)
library(dplyr)

source("R/load_raw_data.r")
source("R/clean_raw_data.r")
source("R/get_stegomyia_indices_by_type_of_study_and_geo.r")
source("R/get_stegomyia_indices_by_type_of_study_and_star_date.r")
source("R/get_stegomyia_indices_by_type_of_study_star_date_and_geo.r")
source("R/bar_trs.r")

path <- "./data-raw/estudio_entomologico.txt"

col_select <- c("Tipo de Estudio",
                 "Jurisdiccion", "Localidad",
                 "Sector", "Fecha de Inicio",
                 "Semana Epidemiologica",
                 "Casas Revisadas",
                 "Casas Positivas",
                 "Total de Recipientes con Agua",
                 "Total de Recipientes Positivos")

df <- load_raw_data(path, col_name = col_select)

df_clean <- clean_raw_data(df)

df_clean
