library(tidyverse)
library(lazyeval)
library(lubridate)
library(ggplot2)
library(rlang)

source("R/clean_raw_data.r")

path <- "./data-raw/estudio_entomologico.txt"

col_select1 <- c("Tipo de Estudio",
                 "Jurisdiccion", "Localidad",
                 "Sector", "Fecha de Inicio",
                 "Semana Epidemiologica",
                 "Casas Revisadas",
                 "Casas Positivas",
                 "Total de Recipientes con Agua",
                 "Total de Recipientes Positivos")

df1 <- clean_raw_data(path, col_name = col_select1)

df1
