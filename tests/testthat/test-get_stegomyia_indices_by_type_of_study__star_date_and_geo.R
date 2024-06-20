# path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
# path_data_file_namet <- "/data-raw/qrt.csv"
# path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
# df <- read_csv(path_datat)
# Make DataFrame


dftest <- data.frame(HI = (9/75*100), CI = (9/168*100), BI = (9/75*100))
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)
test_that("calculation_of_stegomyia_indices_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df0,
                                                     st = "Verificacion",
                                                     date = "2021/01/06",
                                                     var = "540"
                                                     )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)
  }
)
test_that("calculation_0_of_stegomyia_indices_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df0,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/08",
                                                                       var = 390
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
  }
)
test_that("Error_in_calculation_of_stegomyia_indices_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df0,
                                                                        st = "Verificacion",
                                                                        date = "2021/01/08",
                                                                        var = "000"
  ),
    "These filters don´t have data in this data.frame")
}
)
test_that("Warrning_in_calculation_of_stegomyia_indices_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df0,
                                                                      st = "Verificacion",
                                                                      date = "2021/02/12",
                                                                      var = "1248"
  ),
  "Casa_Revisada with 0")
}
)
