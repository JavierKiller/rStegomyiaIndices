# path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
# path_data_file_namet <- "/data-raw/qr_fullt.csv"
# path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")
# df <- read_csv(path_datat)
dftest <- data.frame(Localidad = "HERMOSILLO",
                     Pct_Recipientes_Tratables = (9/45*100),
                     Pct_Recipientes_Controlables = (36/45*100),
                     Pct_Recipientes_Eliminables = (0/45*100)
                     )
dftest0 <- data.frame(HI = 0, CI = 0, BI = 0)

test_that("calculation_of_typology_container_date_of_data.frame", {
  expected <- get_typology_container_by_type_of_study_and_location(df = df_fullt,
                                                                   st = "Verificacion",
                                                                   var = "HERMOSILLO"
  )
  expect_s3_class(expected, "data.frame")
  #expect_equivalent(expected, dftest, tolerance = 0.01)

}
)
test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
  expect_warning(get_typology_container_by_type_of_study_and_location(df = df_fullt,
                                                                        st = "Verificacion",
                                                                        var = "NAVOJOA"
  ),
  "Casa_Revisada with 0")
}
)

#expect_warning()
