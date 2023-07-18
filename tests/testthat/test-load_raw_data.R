path_raw_data <- "./data-raw/estudio_entomologico.txt"



test_that("assess_load_raw_data",
  {
    expected <- load_raw_data(path = path_raw_data )
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(20, 12))
#    expect_named(expected, c("Tipo_de_Estudio", "Clave_Jurisdiccion",
#                             "Jurisdiccion", "Clave_Localidad", "Localidad",
#                             "Sector", "Fecha_de_Inicio",
#                             "Semana_Epidemiologica", "Casas_Revisadas",
#                             "Casas_Positivas", "Total_de_Recipientes_con_Agua",
#                             "Total_de_Recipientes_Positivos"))


#    expect_identical(names(expected), labels)
  }
)


