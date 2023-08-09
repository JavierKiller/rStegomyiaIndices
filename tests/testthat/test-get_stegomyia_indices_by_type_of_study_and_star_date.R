path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)
# Make DataFrame
dftest <- data.frame(HI = 8.510638, CI = 3.658537, BI = 9.574468)
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)
test_that("calculation_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                   st = "Verificacion",
                                                                   date = "2021/01/06"
  )
  expect_s3_class(expected, "data.frame")
  expect_equal(expected$HI, dftest$HI, tolerance = 0.01)
  expect_equal(expected$CI, dftest$CI, tolerance = 0.01)
  expect_equal(expected$BI, dftest$BI, tolerance = 0.01)
 }
)
test_that("calculation_0_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/12"
  )
  expect_s3_class(expected, "data.frame")
  expect_equal(expected$HI, dftest0$HI)
  expect_equal(expected$CI, dftest0$CI)
  expect_equal(expected$BI, dftest0$BI)
}
)
test_that("Error_in_calculation_of_stegomyia_indices_date_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                    st = "Verificacion",
                                                                    date = "2022/01/11"
  ),
  "These filters don´t have data in this data.frame")
}
)
