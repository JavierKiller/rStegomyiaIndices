#library(tidyverse)
#library(lazyeval)
#library(lubridate)

#source("R/get_stegomyia_indices_by_type_of_study_and_geo.r")

df <- read_csv("./data/qr_for_test.csv")  # ,col_types = colt_

test_that("calculate Stegomyia indices for each sampling select study type and
          geographic", {
  col1_ <- c("Tipo_de_Estudio", "Localidad", "Casas_Revisadas",
              "Casas_Positivas", "Total_de_Recipientes_con_Agua",
              "Total_de_Recipientes_Positivos")

  df2 <- read_csv("./data/qr_for_test.csv", col_select = col1_)

dfti <- df %>%
    filter(Tipo_de_Estudio ==  "Verificacion") %>%
    group_by( "Localidad") %>%
    select(var, Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
              CI = sum(Total_de_Recipientes_Positivos)/
                sum(Total_de_Recipientes_con_Agua)*100,
              BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)


  df1 <- get_stegomyia_indices_by_type_of_study_and_geo(
    df,
    st = "Verificacion",
    var = "Localidad"
  )
  expect_equal(df1, dfti)
})



# print(dft_)

