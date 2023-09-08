path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)
# Make DataFrame
dftest <- data.frame(HI = (48/564*100), CI = (54/1476*100), BI = (54/564*100))
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)
test_that("calculation_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                   st = "Verificacion",
                                                                   date = "2021/01/06"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)

 }
)
test_that("calculation_0_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/12"
  )
  expect_s3_class(expected, "data.frame")
  #expect_equivalent(expected, dftest0, tolerance = 0.01)
}
)
# test_that("Error_in_calculation_of_stegomyia_indices_date_of_data.frame", {
#   expect_error(get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
#                                                                     st = "Verificacion",
#                                                                     date = "2021/01/11"
#   ),
#   "These filters don´t have data in this data.frame")
# }
# )
# test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
#   expect_warning(get_typology_container_by_type_of_study_and_location(df = df,
#                                                                       st = "Verificacion",
#                                                                       var = "2021/02/10"
#   ),
#   "Casa_Revisada with 0")
# }
# )
