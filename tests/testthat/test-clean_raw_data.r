path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_name <- "/data-raw/qr.csv"
path_data_file_name_lew <- "/data-raw/qr_lew.csv"
path_data_file_name_full <- "/data-raw/qr_full.csv"
path_data <- paste(path_data_prefix, path_data_file_name, sep = "")
path_data_lew <- paste(path_data_prefix, path_data_file_name_lew, sep = "")
path_data_full <- paste(path_data_prefix, path_data_file_name_full, sep = "")
#path_data <- "../../data-raw/qr.csv"
df <- read.csv(path_data)
df_lew <- read.csv(path_data_lew)
df_full <- read.csv(path_data_full)
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
)
colt_d = list(
  Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
  Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
  Semana_Epidemiologica = "f",
  Casas_Revisadas = "d",
  Casas_Positivas = "d",
  Total_de_Recipientes_con_Agua = "d",
  Total_de_Recipientes_Positivos = "d"
)
test_that("assess_clean_raw_data", {
  expected <- clean_raw_data(df, path_out = path_data)
  expect_identical(str(colt), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(20, 12))
 })

test_that("assess_clean_raw_data_sub", {
  expected <- clean_raw_data(df_lew, path_out = path_data_lew)
  expect_identical(str(colt_d), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(20, 7))
 })
test_that("assess_clean_raw_data_extra", {
  expected <- clean_raw_data(df_full, path_out = path_data_full)
  expect_identical(str(colt), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(20, 19))
})
