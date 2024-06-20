#path_datam <- ("data-raw/statusindicesector.csv")
#df <- read_csv(path_datam)

#dfempty

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  }
)

test_that("load dataframe", {
    expect_s3_class(dfm, "data.frame")
  }
)
test_that(
  "error_get_maps_stegomyia_indices",
  {
    expect_error(
      get_maps_stegomyia_indices(df = df0),
      "dataframe o path is incorrect"
    )
    # expect_error(
    #   get_maps_stegomyia_indices(df = dfempty),
    #   "dataframe is empty"
    # )
  }
)
