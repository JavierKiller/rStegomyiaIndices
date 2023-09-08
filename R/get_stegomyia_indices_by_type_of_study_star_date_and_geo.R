#' Get stegomyia indices by type of study, date and geo
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type, date and geographic variable using the
#' following
#'  formulas:
#'       - Container Index(CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#' @param df the dataframe with information.
#' @param st The type of study selected. By default, it is set to
#' "Verificacion".
#' @param date The date used to calculate the stegomyia indices. By default,
#' it is set to "2021/01/07".
#' @param var The geographic variable used to calculate the stegomyia indices.
#'
#' @return
#'
#' The dataframe with stegomyia indices of the selection of type of study and
#' geographic variable.
#'
#' @examples
#' get_stegomyia_indices_by_type_of_study__star_date_and_geo(df, "Encuesta",
#' "2021/01/07, var = "390")
#'
#'  @export

get_stegomyia_indices_by_type_of_study_star_date_and_geo <- function(
    df,
    st ="Verificacion",
    date = "2021/01/07",
    var
)
{
  df$Fecha_de_Inicio <- as.Date(date, format = "%Y/%m/%d")
  filtered_df <- df %>%
    filter(Tipo_de_Estudio == st,
           Fecha_de_Inicio == ymd(date),
           Sector == var)
  condicion_nrows <- nrow(filtered_df)>0
  if (isFALSE(condicion_nrows)){
    stop("These filters don´t have data in this data.frame")
  }
  condiction <- nrow(filtered_df %>%
                       filter(Casas_Revisadas == 0))
  if (condiction !=0){
    warning("Casa_Revisada with 0")
    #print(condiction)
    filtered_df <- filtered_df %>%
      filter(Casas_Revisadas != 0)
  }
  dfti <- filtered_df %>%
    filter(Tipo_de_Estudio ==  st,
           Fecha_de_Inicio == ymd(date),
           Sector == var) %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      CI = if(sum(Total_de_Recipientes_Positivos)>0){
        sum(Total_de_Recipientes_Positivos)/
          sum(Total_de_Recipientes_con_Agua)*100
      }
      else{0},
      BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)
}
