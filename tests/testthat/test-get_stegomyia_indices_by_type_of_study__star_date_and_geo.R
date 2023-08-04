path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
df <- read_csv(path_datat)

# Make DataFrame
dftest <- data.frame(ICP = 12, IRP = 5.3571429, IB = 12)
dftest0 <- data.frame(ICP = 0, IRP = 0, IB = 0)
test_that("multiplication works", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df,
                                                     st = "Verificacion",
                                                     date = "2021/01/06",
                                                     var = 540
                                                     )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest)
  }
)
test_that("multiplication works", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/08",
                                                                       var = 390
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0)
  expect_equal(expected$ICP, dftest0$ICP)
  expect_equal(expected$IRP, dftest0$IRP)
  expect_equal(expected$IB, dftest0$IB)
  }
)
