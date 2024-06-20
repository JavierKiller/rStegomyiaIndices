#' Get stegomyia indices by type of study and location
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type and location variable using the
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
#' @param var The location variable used to calculate the stegomyia indices.
#'
#' @return
#'
#' The dataframe with stegomyia indices of the selection of type of study and
#' location variable.
#'
#' @examples
#'
#' get_stegomyia_indices_by_type_of_study_and_loc(df0, st = "Verificacion", "CIUDAD_OBREGÓN")
#'

####TODO: actualizar con vignette####
get_stegomyia_indices_by_type_of_study_and_loc <- function(
    df,
    st = "Verificacion",
    var = c("CIUDAD_OBREGÓN","HERMOSILLO")
) {
  filtered_df <- df %>%
    filter(Tipo_de_Estudio == st, Localidad %in% var)

  if (nrow(filtered_df) == 0) {
    stop("These filters don't have data in this data.frame")
  }
  if (any(filtered_df$Casas_Revisadas == 0)) {
    warning("Casa_Revisada with 0")
    filtered_df <- filtered_df %>%
      filter(Casas_Revisadas != 0)
  }

  dfti <- filtered_df %>%
    dplyr::select(Localidad,
           Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    group_by( Localidad ) %>%
    summarize(
      HI = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
      CI = if (sum(Total_de_Recipientes_Positivos) > 0) {
        sum(Total_de_Recipientes_Positivos) / sum(Total_de_Recipientes_con_Agua) * 100
      } else {
        0
      },
      BI = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
    ) %>%
    ungroup()

  return(dfti)
}
