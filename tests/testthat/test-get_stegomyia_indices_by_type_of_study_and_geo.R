path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)
# Make DataFrame
dftest <- data.frame(HI = 3.10, CI = 1.38, BI = 3.32)
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("calculation_of_stegomyia_indices_by_type_of_study_and_geo_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_geo(df = df,
                                                                   st = "Verificacion",
                                                                   var = "Localidad"
  )
   expect_s3_class(expected, "data.frame")
  expect_equal(expected$HI, dftest$HI, tolerance = 0.01)
  expect_equal(expected$CI, dftest$CI, tolerance = 0.01)
  expect_equal(expected$BI, dftest$BI, tolerance = 0.01)
}
)

