path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)
test_that("calculation_of_stegomyia_indices_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df1 = df,
                                                                   st = "Verificacion",
                                                                   date = "2021/01/07"
  )
  expect_s3_class(expected, "data.frame")
  #expect_equivalent(expected, dftest)
 }
)
