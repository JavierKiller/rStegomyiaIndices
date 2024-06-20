# path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
# path_data_file_namet <- "/data-raw/qrt.csv"
# path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
# df <- read_csv(path_datat)

# Make DataFrame
dftest <- data.frame(Sector = factor(c(569, 840)),
                     HI = c((24/123*100), (24/123*100)),
                     CI = c((45/1176*100), (45/1176*100)),
                     BI = c((45/123*100), (45/123*100)))
dftest0 <- data.frame(Sector = factor(c(500, 927)),
                      HI = c(0, 0),
                      CI = c(0, 0),
                      BI = c(0, 0))
#st = "Verificacion"
#date = "2021/01/06"
test_that("calculation_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df0t,
                                                                   st = "Encuesta",
                                                                   date = "2021/01/04"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)

 }
)
test_that("calculation_0_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_star_date(df0t,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/11"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
}
)
test_that("Error_in_calculation_of_stegomyia_indices_date_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_star_date(df0t,
                                                                    st = "Verificacion",
                                                                    date = "2021/04/11"
  ),
  "These filters don´t have data in this data.frame")
}
)
test_that("warning_in_calculation_of_stegomyia_indices_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_and_star_date(df0t,
                                                                      st = "Verificacion",
                                                                      date = "2021/02/12"
  ),
  "Casa_Revisada with 0")
}
)
