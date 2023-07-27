path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices"
path_data_file_name <- "/data-raw/qr.csv"
path_data <- paste(path_data_prefix, path_data_file_name, sep = "")
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
