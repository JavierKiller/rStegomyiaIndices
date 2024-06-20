
path <- "data-raw/estudio_entomologico.txt"

 col_select <- c("Tipo de Estudio",
                  "Jurisdiccion", "Localidad",
                  "Sector", "Fecha de Inicio",
                  "Semana Epidemiologica",
                  "Casas Revisadas",
                  "Casas Positivas",
                  "Total de Recipientes con Agua",
                  "Total de Recipientes Positivos")

 dflrd <- load_raw_data(path, col_name = col_select)


## Diccionario de variables de la base de datos CSV

dfdir <-read_csv("~/CursoQR/Package1/diccionario_estudio_entomologico.csv")
print(head(dfdir,20))


path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices"
path_data_file_name <- "/data-raw/qr.csv"
path_data <- paste(path_data_prefix, path_data_file_name, sep = "")

df <- read_csv(path_data)

df_ <- clean_raw_data(df)

path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices"
path_data_file_name <- "/data-raw/qr.csv"
path_data <- paste(path_data_prefix, path_data_file_name, sep = "")
#path <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices/data/qr_for_test.csv"
df <- read.csv(path_data)
test_that("assess_clean_raw_data", {
  expected <- clean_raw_data(df)
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(20, 12))
  # TODO probar tipos de datos  de colt ####
  #TODO porbar tipos de datos de subconjuntos de colt ####
  #TODO probar archivos de salida .csv para colt ####
  #TODO probar archivos de salida .csv para subconjunto de colt ####
})
importFrom(tidyverse)
importFrom(lazyeval)
importFrom(lubridate)
importFrom(rlang)
importFrom(dplyr)
importFrom(ggplot2)
importFrom(readr)

# "tidyverse",
# "lazyeval",
# "lubridate",
# "rlang",
# "dplyr",
# "ggplot2",
# "readr"
#
# tidyverse,
# lazyeval,
# lubridate,
# rlang,
# dplyr,
# ggplot2,
# readr

col_namef = cols(
  Tipo_de_Estudio = col_factor(levels = c("Encuesta", "Verificacion")),
  Clave_Jurisdiccion = col_factor(levels = as.character(c(2601, 2602, 2603, 2604, 2605, 2606))),
  Jurisdiccion = col_factor(levels = c(
    "Hermosillo", "Caborca", "Cananea", "Cajeme", "Navojoa", "San_Luis_Río_Colorado"
  )),
  Clave_Municipio = col_factor(levels = as.character(1:72)),
  Municipio = col_factor(),
  Clave_Localidad = col_factor(),
  Localidad = col_factor(),
  Sector = col_factor(levels = as.character(1:2000)),
  Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
  Semana_Epidemiologica = col_factor(levels = as.character(1:53)),
  Casas_Revisadas = col_double(),
  Casas_Positivas = col_double(),
  Total_de_Recipientes_con_Agua = col_double(),
  Total_de_Recipientes_Positivos = col_double(),
  Total_de_Recipientes_Positivos_a_Pupas= col_double(),
  No._Total_de_Pupas_en_Recipientes = col_double(),
  Recipientes_Tratables = col_double(),
  Recipientes_Controlables = col_double(),
  Recipientes_Eliminables = col_double()
)

df1 <- read_csv("data/qr_for_test.csv")

show_col_types = TRUE
dft_ <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df, st = "Verificacion",
                                            date = "2021/01/06", var = 540)
print(dft_)

icp <-9/75*100
irp <- 9/168*100
ib <- 9/168*100
#ICP      IRP      IB
#  12    5.357143  12

col_namef = c(
  "Tipo de Estudio",
  "Jurisdiccion",
  "Municipio",
  "Localidad",
  "Sector",
  "Fecha de Inicio",
  "Semana Epidemiologica",
  "Casas Revisadas",
  "Casas Positivas",
  "Total de Recipientes con Agua",
  "Total de Recipientes Positivos",
  "Total de Recipientes Positivos a Pupas",
  "No. Total de Pupas en Recipientes",
  "Recipientes Tratables",
  "Recipientes Controlables",
  "Recipientes Eliminables"
)


sd <- read_tsv(path_raw_data,locale = locale(encoding = "UTF-16" ))
str(sd)
df_full <- load_raw_data(path_raw_data, col_namef)
write_csv(df_full, "~/CursoQR/Package1/rStegomyiaIndices/data-raw/qr_full.csv")

col_namelew = c(
  "Tipo de Estudio",
  "Fecha de Inicio",
  "Semana Epidemiologica",
  "Casas Revisadas",
  "Casas Positivas",
  "Total de Recipientes con Agua" ,
  "Total de Recipientes Positivos"
 )

df_lew <- load_raw_data(path_raw_data, col_namelew)
write_csv(df_lew, "~/CursoQR/Package1/rStegomyiaIndices/data-raw/qr_lew.csv")


df_ <- load_raw_data(path_raw_data)
write_csv(df_, "~/CursoQR/Package1/rStegomyiaIndices/data-raw/qr.csv")

get_stegomyia_indices_by_type_of_study_star_date_and_geo(path_datat)
#test funcionn
dft_<-get_breteau_idx_by_te_star_date(df1, te = "Verificacion",
                                      fecha = "2021/01/06" )
dft_

#dft_<- get_breteau_idx_by_te_geo(df1, te= "Encuesta", var="Sector")

print(dft_)

# 8.510638 3.658537 9.574468
# 8.510638 3.658537 9.574468

dfg <-get_stegomyia_indices_by_type_of_study_and_geo(df, st = "Verificacion", var = "401")
dfg


hi<- (24/282)*100
ci<- (27/738)*100
bi<- (27/282)*100
hi
ci
bi



## Diccionario de variables de la base de datos CSV

dfdir <-read_csv("~/CursoQR/Package1/diccionario_estudio_entomologico.csv")
print(head(dfdir,20))

usethis::use_data(dfdir)
# Características principales de la base de datos:
#
# - Contiene 121 variables diferentes.
# - Datos creados con la misma estructura y tipo de datos.
# -
# - Etc.

get_stegomyia_indices_by_type_of_study_and_geo <- function( df,
                                                            st ="Verificacion",
                                                            var){

  # condicion_var <- var %in% colnames(df)
  # if (isFALSE(condicion_var)){
  #    stop("The var selection is incorre")
  # }
  # dfd <- df %>%
  #    filter(Tipo_de_Estudio == st, !!sym(var))
  # if (nrow(dfd) == 0){
  #    stop("These filters don´t have data in this data.frame")
  # }
  dfti <- df %>%
    filter(Tipo_de_Estudio ==  st) %>%
    group_by( !!sym(var)) %>%
    select(var, Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
              CI = sum(Total_de_Recipientes_Positivos)/
                sum(Total_de_Recipientes_con_Agua)*100,
              BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)

}



f <- function(x) {
  if (x < 0) {
    warning("*x* is already negative")
    return(x)
  }
  -x
}

x <- factor(c("single", "married", "married", "single"))
print(x)

x <- factor(c("single", "married", "married", "single"), levels = c("single", "married", "divorced"))
print(x)


df_fullt <- clean_raw_data(df=df_full, path_out = "~/CursoQR/Package1/rStegomyiaIndices/data-raw/qr.csv",
                           col_name = col_namef)


get_typology_container_by_type_of_study_and_location(df = df_fullt,
                                                     st = "Verificacion",
                                                     var = "HERMOSILLO")


var0 <- c("1248", "401")
resultado <- get_stegomyia_indices_by_type_of_study_and_geo(df = df0t, st = "Verificacion", var = var0)

rdd <-get_stegomyia_indices_by_type_of_study_and_geo(df = df0t,
                                                     st = "Verificacion",
                                                     var = "001"
)




get_stegomyia_indices_by_type_of_study_and_geo <- function(
    df,
    st ="Verificacion",
    var
)
{
  filtered_df <- df %>%
    filter(Tipo_de_Estudio == st, Sector %in% var)
  condiction <- nrow(filtered_df %>%
                       filter(Casas_Revisadas == 0))

  condicion_nrows <- nrow(filtered_df)>0
  if (isFALSE(condicion_nrows)){
    stop("These filters don´t have data in this data.frame")
  }
  if (condiction !=0){
    warning("Casa_Revisada with 0")
    filtered_df <- filtered_df %>%
      filter(Casas_Revisadas != 0)
  }
  dfti <- filtered_df %>%
    filter(Tipo_de_Estudio ==  st, Sector %in% var) %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      CI = if(sum(Total_de_Recipientes_Positivos)>0){
        sum(Total_de_Recipientes_Positivos)/
          sum(Total_de_Recipientes_con_Agua)*100
      }
      else{0},
      BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)
}
EX <- get_stegomyia_indices_by_type_of_study_and_loc(df=df0t)

xx <- get_stegomyia_indices_by_type_of_study_epidemic_week_and_loc()

aa <-c(390, 400, 401, 403, 444, 500, 513, 540, 569)
"Verificacion"
get_stegomyia_indices_by_type_of_study_and_geo(df0t, st = "Encuesta", var = 915)

dfx <- get_stegomyia_indices_by_type_of_study_and_geo_is(df0t, st = "Encuesta", var = aa)

col_name = cols(Sector = col_double())
path_out="~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv"
df_aux <- read_csv(
  path_out,
  col_types = col_name)
write_csv(df_aux, "~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector0.csv")
view(df0t)
#ejemplo con otras bases de datos

write_csv(df_ev, "~/CursoQR/Package1/rStegomyiaIndices/data-raw/alterado.csv")
df_evalterado <- read.csv("~/CursoQR/Package1/rStegomyiaIndices/data-raw/alterado.csv")
df_evalterado
df_evt11 <- df_evalterado %>%
  group_by(Sector) %>%
  summarize(
    #Days = (Fecha_de_Inicio_V - Fecha_de_Inicio_E),
    HI_E = sum(Casas_Positivas_E) / sum(Casas_Revisadas_E) * 100,
    HI_V = sum(Casas_Positivas_V) / sum(Casas_Revisadas_V) * 100,
    CI_E = if (sum(Total_de_Recipientes_Positivos_E) > 0) {
      sum(Total_de_Recipientes_Positivos_E) / sum(Total_de_Recipientes_con_Agua_E) * 100
    } else {
      0
    },
    CI_V = if (sum(Total_de_Recipientes_Positivos_V) > 0) {
      sum(Total_de_Recipientes_Positivos_V) / sum(Total_de_Recipientes_con_Agua_V) * 100
    } else {
      0
    },
    BI_E = sum(Total_de_Recipientes_Positivos_E) / sum(Casas_Revisadas_E) * 100,
    BI_V = sum(Total_de_Recipientes_Positivos_V) / sum(Casas_Revisadas_V) * 100
  ) %>%
  ungroup()
df_evt11


path_raw_data

#"data-raw/estudio_entomologico1.txt"

path_raw_data_error

#"data-raw/estudio_entomologico_equivocado.txt"
path_raw_data_error <- "data-raw/estudio_entomologico_equivocado.txt"
usethis::use_data(path_raw_data_error, overwrite = TRUE)

path_raw_data_error

#"data-raw/data-raw/estudio_entomologico.txt"
path <- "data-raw/estudio_entomologico1.txt"
usethis::use_data(path, overwrite = TRUE)

path_raw_data_empty

#"data-raw/estudio_entomologico_sin_datos.txt"
path_raw_data_empty <- "data-raw/estudio_entomologico_sin_datos.txt"
usethis::use_data(path_raw_data_empty, overwrite = TRUE)

labels

labels_sub

path_data_lew

#qr_lew.csv
path_data_lew <- "data-raw/qr_lew.csv"
usethis::use_data(path_data_lew, overwrite = TRUE)

#df_lew <- df_lewt son diferentes
usethis::use_data(df_lew, overwrite = TRUE)

path_data_full

#qr_full.csv
path_data_full <- "data-raw/qr_full.csv"
usethis::use_data(path_data_full, overwrite = TRUE)

df_full <- df_fullt
usethis::use_data(df_full, overwrite = TRUE)

path_data

#qr.csv
path_data <- "data-raw/qrt.csv"
usethis::use_data(path_data, overwrite = TRUE)

df0 <- df0t
usethis::use_data(df0, overwrite = TRUE)

path_data_lewt

#qr_lewt.csv

df_lewt

path_data_fullt

#qr_fullt.csv

df_fullt

path_datat

#qrt.csv

df0t

dfm

#statusindicesector.csv

#ejercicio_sectores_hermosillo.shp

w1 <- st_read("data-raw/maps/ejercicio_sectores_hermosillo.shp")
w1
usethis::use_data(w1, overwrite = TRUE)
#ejercicio_sectores_hermosillo2.shp

#d0=readShapePoly("data-raw/maps/ejercicio_sectores_hermosillo2.shp")
d0 <- st_read("data-raw/maps/ejercicio_sectores_hermosillo2.shp")

d0
usethis::use_data(d0, overwrite = TRUE)

dflrd
usethis::use_data(dflrd, overwrite = TRUE)

#get_stegomyia_indices_by_type_of_study_and_geo_is(df0, st = "Verificacion", var=)
