library(testthat)
library(tidyverse)

#source("R/get_stegomyia_indices_by_type_of_study_and_geo.r")

df <- read_csv("./data/qr_for_test.csv")
df0 <- df %>%
  mutate(Casas_Positivas = 0, Total_de_Recipientes_Positivos = 0)

test_that("calculate Stegomyia indices for each sampling select study type and geographic", {
  col1_ <- c("Tipo_de_Estudio", "Localidad", "Casas_Revisadas",
              "Casas_Positivas", "Total_de_Recipientes_con_Agua",
              "Total_de_Recipientes_Positivos")

  df2 <- read_csv("./data/qr_for_test.csv", col_select = col1_)

  df2_  <- df2 %>%
    filter(Tipo_de_Estudio ==  "Verificacion") %>%
    group_by( Localidad) %>%
    select(Localidad, Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
              CI = sum(Total_de_Recipientes_Positivos)/
                sum(Total_de_Recipientes_con_Agua)*100,
              BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()

  df1 <- get_stegomyia_indices_by_type_of_study_and_geo(
    df,
    st = "Verificacion",
    var = "Localidad"
  )
  expect_identical(df1, df2_)

})

test_that("calculate Stegomyia indices with 0, for each sampling select study type and geographic", {

  df2_  <- df0 %>%
    filter(Tipo_de_Estudio ==  "Verificacion") %>%
    group_by( Localidad) %>%
    select(Localidad, Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
              CI = sum(Total_de_Recipientes_Positivos)/
                sum(Total_de_Recipientes_con_Agua)*100,
              BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()

  df1 <- get_stegomyia_indices_by_type_of_study_and_geo(
    df0,
    st = "Verificacion",
    var = "Localidad"
  )
  expect_identical(df1, df2_)

})
