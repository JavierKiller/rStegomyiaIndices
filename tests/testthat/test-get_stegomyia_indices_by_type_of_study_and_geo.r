library(tidyverse)
library(lazyeval)
library(lubridate)

source("R/get_stegomyia_indices_by_type_of_study_and_geo.r")

colt_<- "fffffDfdddd" # arreglar esto

df1 <- read_csv(".../data/qr_for_test.csv", col_types = colt_ )

dft_ <- get_stegomyia_indices_by_type_of_study_and_geo(
  df1,
  te = "Verificacion",
  var = "clave_Localidad"
)

# print(dft_)

