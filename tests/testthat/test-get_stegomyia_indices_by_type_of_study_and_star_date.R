path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)
# Make DataFrame
dftest <- data.frame(HI = 8.510638, CI = 3.658537, BI = 9.574468)
test_that("calculation_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
                                                                   st = "Verificacion",
                                                                   date = "2021/01/06"
  )
  expect_s3_class(expected, "data.frame")
  #expect_equivalent(expected, dftest)
  expect_equivalent(round(expected$HI, 6), round(dftest$HI, 6))
  expect_equivalent(round(expected$CI, 6), round(dftest$CI, 6))
  expect_equivalent(round(expected$BI, 6), round(dftest$BI, 6))
 }
)
# test_that("Error_in_calculation_of_stegomyia_indices_date_of_data.frame", {
#   expect_error(get_stegomyia_indices_by_type_of_study_and_star_date(df = df,
#                                                                     st = "Verificacion",
#                                                                     date = "2022/01/08",
#   ),
#   "These filters don´t have data in this data.frame")
# }
# )
