


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
#    names_expect <- names(expected)
    expect_named(expected, labels)
  }
)
