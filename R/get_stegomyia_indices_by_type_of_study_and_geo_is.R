#' Get stegomyia indices by type of study  geo and indices status
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_raw_data" and changed type data of function
#' "clean_raw_data", select study type and geographic variable using the
#' following
#'    formulas:
#'       - Container Index(CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#'
#' @param df the dataframe with information.
#' @param st The type of study selected. By default, it is set to
#' "Verificacion".
#' @param var The geographic variable used to calculate the stegomyia indices.
#' @param path_out path for that data of stegomyia indices and status. By
#' default, it is set to "~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv"
#'
#' @return
#'
#' The dataframe with stegomyia indices and status of the selection of type of
#' study and geographic variable.
#'
#' @export
#'
#' @examples
#' get_stegomyia_indices_by_type_of_study_and_geo_is(df, st = "Verificacion",
#'   var ="390").
#'

####TODO: actualizar con vignette####
get_stegomyia_indices_by_type_of_study_and_geo_is <- function(
    df,
    st = "Verificacion",
    var,
    path_out="~/CursoQR/Package1/rStegomyiaIndices/data-raw/statusindicesector.csv"
) {
  filtered_df <- df %>%
    filter(Tipo_de_Estudio == st, Sector %in% var)

  if (nrow(filtered_df) == 0) {
    stop("These filters don't have data in this data.frame")
  }
  if (any(filtered_df$Casas_Revisadas == 0)) {
    warning("Casa_Revisada with 0")
    filtered_df <- filtered_df %>%
      filter(Casas_Revisadas != 0)
  }

  dfti <- filtered_df %>%
    dplyr::select(Sector,
           Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    group_by( Sector ) %>%
    summarize(
      HI = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
      CI = if (sum(Total_de_Recipientes_Positivos) > 0) {
        sum(Total_de_Recipientes_Positivos) / sum(Total_de_Recipientes_con_Agua) * 100
      } else {
        0
      },
      BI = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
    ) %>%
    mutate(
      index_status_HI = case_when(
        HI == 0 ~ "Optimo",
        HI > 0 & HI <= 1 ~ "Optimo",
        HI > 1 & HI<= 3 ~ "Bueno",
        HI > 3 & HI <= 5 ~ "Alarma",
        is.na(HI) ~ "n",
        .default =  "Emergencia"
      ),
      index_status_CI = case_when(
        CI == 0 ~ "Optimo",
        CI > 0 & CI <= 1 ~ "Optimo",
        CI > 1 & CI<= 3 ~ "Bueno",
        CI > 3 & CI <= 5 ~ "Alarma",
        is.na(CI) ~ "n",
        .default =  "Emergencia"
      ),
      index_status_BI = case_when(
        BI == 0 ~ "Optimo",
        BI > 0 & BI <= 1 ~ "Optimo",
        BI > 1 & BI<= 3 ~ "Bueno",
        BI > 3 & BI <= 5 ~ "Alarma",
        is.na(BI) ~ "n",
        .default =  "Emergencia"
      )
    )%>%
    ungroup()
  df <- dfti
  write_csv(df, path_out)
   return(dfti)
}

