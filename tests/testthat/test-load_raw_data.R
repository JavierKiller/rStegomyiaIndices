path_raw_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices"
path_raw_data_file_name <- "/data-raw/estudio_entomologico.txt"
path_raw_data <- paste(path_raw_data_prefix, path_raw_data_file_name, sep = "")
prdefile_name <- "/data-raw/estudio_entomologico_equivocado.txt"
path_raw_data_error <- paste(path_raw_data_prefix,prdefile_name, sep = "")
prdemfile_name <- "/data-raw/estudio_entomologico_sin_datos.txt"
path_raw_data_empty <- paste(path_raw_data_prefix,prdemfile_name, sep = "")
labels <- c(
  "Tipo_de_Estudio",
  "Clave_Jurisdiccion",
  "Jurisdiccion",
  "Clave_Localidad",
  "Localidad",
  "Sector",
  "Fecha_de_Inicio",
  "Semana_Epidemiologica",
  "Casas_Revisadas",
  "Casas_Positivas",
  "Total_de_Recipientes_con_Agua",
  "Total_de_Recipientes_Positivos")
labels_sub <- c(
  "Tipo_de_Estudio",
  "Semana_Epidemiologica",
  "Casas_Revisadas",
  "Casas_Positivas",
  "Total_de_Recipientes_con_Agua",
  "Total_de_Recipientes_Positivos")
col_select_sub <- c("Tipo de Estudio",
                    "Semana Epidemiologica",
                    "Casas Revisadas",
                    "Casas Positivas",
                    "Total de Recipientes con Agua",
                    "Total de Recipientes Positivos")
test_that(
  "assess_load_raw_data_error_Td",
  {
    expect_error(
      load_raw_data(path = path_raw_data_error),
      "data file .txt dont have Tipo de Estudio o path is incorrect"
    )
    expect_error(
      load_raw_data(path = path_raw_data_empty),
      "data file .txt is empty"
    )
  }
)

test_that("assess_load_raw_data",
  {
    expected <- load_raw_data(path = path_raw_data )
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(20, 12))
    expect_named(expected, labels)
  }
)
test_that(
  "assess_load_raw_data_labels_sub",
  {
    expected <- load_raw_data(path = path_raw_data, col_name = col_select_sub )
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(20, 6))
    expect_named(expected, labels_sub)
  }
)
