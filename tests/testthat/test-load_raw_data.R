path_raw_data <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices/data-raw/estudio_entomologico.txt"
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
test_that("assess_load_raw_data",
  {
    expected <- load_raw_data(path = path_raw_data )
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(20, 12))
    expect_named(expected, labels)
  }
)
# TODO si no tiene la variable "Tipo de Estudio" no se puede trabajar con la base de datos ####
# TODO probar sub conjuntos con de labels ####
col_select_sub <- c("Tipo de Estudio",
                    "Semana Epidemiologica",
                    "Casas Revisadas",
                    "Casas Positivas",
                    "Total de Recipientes con Agua",
                    "Total de Recipientes Positivos")
labels_sub <- c(
  "Tipo_de_Estudio",
  "Semana_Epidemiologica",
  "Casas_Revisadas",
  "Casas_Positivas",
  "Total_de_Recipientes_con_Agua",
  "Total_de_Recipientes_Positivos")
test_that("assess_load_raw_data",
          {
            expected <- load_raw_data(path = path_raw_data, col_name = col_select_sub )
            expect_s3_class(expected, "data.frame")
            expect_equal(dim(expected), c(20, 6))
            expect_named(expected, labels_sub)
          }
)
# TODO probar con archivos que no sean el la estructura del archivo a trabajar ####
