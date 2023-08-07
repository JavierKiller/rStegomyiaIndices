#'  Get stegomyia indices by type of study and star date
#'  @description
#'  The Stegomyia indices are calculated for each sampling select study type
#'  and date using the following formulas:
#'  * `Container Index(CI)`: number of infected containers × 100/total number
#'     of containers
#'  * `House Index (HI)`: number of infected houses × 100/total number of
#'     houses
#'  * `Breteau Index (BI)`: number of positive containers/number of houses
#'     explored × 100
#'  @param `df` the dataframe with information
#'  @param `st` The type of study selected. By default, it is set to
#'    "Verificacion"
#'  @param `date` The date used to calculate the stegomyia indices. By default,
#'   it is set to "2021/01/07".
#'  @return
#'  The dataframe with Stegomyia indices of the selection of type of study and
#'    date.
#' @export
#' @examples
#'  df<-get_stegomyia_indices_by_type_of_study_and_star_date(df, st = "Verificacion",
#'  date = "2021/01/06" )
#'
get_stegomyia_indices_by_type_of_study_and_star_date <- function(
    df,
    st = "Verificacion",
    date = "2021/01/07"
){
  dfd <- df %>%
    filter(Tipo_de_Estudio ==  st, Fecha_de_Inicio == ymd(date))
  condicion_nrows <- nrow(dfd)>0
  if (isFALSE(condicion_nrows)){
    stop("These filters don´t have data in this data.frame")
  }
  dfti <- dfd %>%
    filter(Tipo_de_Estudio ==  st, Fecha_de_Inicio == ymd(date)) %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      CI = sum(Total_de_Recipientes_Positivos)/
        sum(Total_de_Recipientes_con_Agua)*100,
      BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)

}
