library(tidyverse)
library(lazyeval)
library(lubridate)

#source("R/clean_raw_data.r")
path <- "./data-raw/estudio_entomologico.txt"
path_out <- "./data/qr2.csv"

#df1 <- clean_raw_data(path)

col1_ <- c("Tipo de Estudio",
           "Jurisdiccion",
           "Localidad",
           "Sector",
           "Fecha de Inicio",
           "Semana Epidemiologica",
           "Casas Revisadas",
           "Casas Positivas",
           "Total de Recipientes con Agua" ,
           "Total de Recipientes Positivos")

  df2 <- read_tsv(
    path,
    col_select = col1_,
    locale = locale(encoding = "UTF-16" ))
colnames(df2) <- str_replace_all(colnames(df2), pattern = " ", replacement = "_" )

  df2 <- df2 %>%
    separate(Localidad, into = c("clave_Localidad", "Localidad"), sep = " ")

  colt = list(
    Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
    Jurisdiccion = "f",
    clave_Localidad = "f",
    Localidad = "f",
    Sector = "f",
    Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
    Semana_Epidemiologica = "f",
    Casas_Revisadas = "d",
    Casas_Positivas = "d",
    Total_de_Recipientes_con_Agua = "d",
    Total_de_Recipientes_Positivos = "d"

  )

  write_csv(df2, path_out)

  df2 <- read_csv(
    path_out,
    col_types = colt,
  )

test_that(" return a data frame", {

  df1 <- clean_raw_data(path)

  expect_equal(df1, df2)
})
