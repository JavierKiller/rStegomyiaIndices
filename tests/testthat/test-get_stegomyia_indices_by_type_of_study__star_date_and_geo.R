path_data_prefix <- "~/CursoQR/Package1/rStegomyiaIndices"
path_data_file_namet <- "/data-raw/qrt.csv"
path_datat <- paste(path_data_prefix, path_data_file_namet, sep = "")

df <- read_csv(path_datat)

show_col_types = TRUE
#ICP      IRP      IB
#  12    5.357143  12

test_that("multiplication works", {
  expect_equal<- get_stegomyia_indices_by_type_of_study__star_date_and_geo(df,
                                                     st = "Verificacion",
                                                     fecha = "2021/01/06",
                                                     sc = 540
                                                     )
})
