#'  Get stegomyia indices by type of study and star date
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type and date variable using the following
#'  formulas:
#'       - Container Index(CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#' @param df the dataframe with information
#' @param st The type of study selected. By default, it is set to
#' "Verificacion"
#' @param date The date used to calculate the stegomyia indices. By default,
#' it is set to "2021/01/07".
#'
#' @return
#'
#' The dataframe with Stegomyia indices of the selection of type of study and
#' date.
#'
#' @export
#' @examples
#'
#' df<-get_stegomyia_indices_by_type_of_study_and_star_date(df,
#' st = "Verificacion", date = "2021/01/06" )

get_stegomyia_indices_by_type_of_study_and_star_date <- function(
    df,
    st = "Verificacion",
    date = "2021/01/07"
){
  date <- as.Date(date, format = "%Y/%m/%d")
  dfd <- df %>%
    filter(Tipo_de_Estudio == st, Fecha_de_Inicio == ymd(date))
  if (nrow(dfd) == 0){
    stop("These filters don´t have data in this data.frame")
  }
  condiction <- nrow(dfd %>%
                       filter(Casas_Revisadas == 0))
  if (condiction !=0){
    print("Error: Casa_Revisada with 0", condiction)
    dfd <- dfd %>%
      filter(Casas_Revisadas != 0)
  }
  dfti <- dfd %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      HI = sum(Casas_Positivas)/sum(Casas_Revisadas)*100,
      CI = if(sum(Total_de_Recipientes_Positivos)>0){
        sum(Total_de_Recipientes_Positivos)/
          sum(Total_de_Recipientes_con_Agua)*100
      }
      else{0},
      BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    ) %>%
    ungroup()
  return(dfti)
}
