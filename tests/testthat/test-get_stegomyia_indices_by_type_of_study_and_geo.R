# path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
# path_data_file_namet <- "/data-raw/qrt.csv"
# path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
# df0 <- read_csv(path_datat)
# df <- clean_raw_data(df0,path_out = path_datat)
#dos fails
var0 <- c("401","1248")
# Make DataFrame
dftest <- data.frame(HI = (3/66*100), CI = (3/156*100), BI = (3/66*100))
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)
test_that("calculation_of_stegomyia_indices_by_type_of_study_and_geo_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_geo(df = df0t,
                                                                   st = "Verificacion",
                                                                   var = "401"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)
 }
)
test_that("calculation_of_stegomyia_indices_0_by_type_of_study_and_geo_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_geo(df = df0t,
                                                             st = "Verificacion",
                                                             var = "390"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
  }
)
test_that("Error_in_calculation_of_stegomyia_indices_by_type_of_study_and_geo_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_geo(df = df0t,
                                                                        st = "Verificacion",
                                                                        var = "001"
  ),
  "These filters don't have data in this data.frame")
}
)
test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_and_geo(df = df0t,
                                                                      st = "Verificacion",
                                                                      var = var0
  ),
  message = "Casa_Revisada with 0")
}
)
