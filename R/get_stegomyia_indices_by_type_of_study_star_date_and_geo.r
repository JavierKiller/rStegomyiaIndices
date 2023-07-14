#' Get stegomyia indices by type of study, date and geo
#'
#'  @description
#'  The Stegomyia indices are calculated for each sampling select study type,
#'  date and geographic variable using the following formulas:
#'
#'  * `Container Index(CI)`: number of infected containers × 100/total number
#'     of containers
#'  * `House Index (HI)`: number of infected houses × 100/total number of
#'     houses
#'  * `Breteau Index (BI)`: number of positive containers/number of houses
#'     explored × 100
#'  @param `df` the dataframe with information
#'  @param `st` The type of study selected. By default, it is set to
#'    "Verificacion"
#'  @param `var` The geographic variable used to calculate the stegomyia
#'    indices
#'  @param `date` The date used to calculate the stegomyia indices. By default,
#'   it is set to "2021/01/07".
#'  @return
#'  The dataframe with stegomyia indices of the selection of type of study and
#'    geographic variable.
#'  @examples
#'  get_stegomyia_indices_by_type_of_study__star_date_and_geo(df, "Encuesta", "2021/01/07", "Sector")
#'  @export



get_stegomyia_indices_by_type_of_study__star_date_and_geo <- function( df, st ="Verificacion",
                                                 date = "2021/01/07", var){

  dfti <- df %>%
    filter(Tipo_de_Estudio ==  te, Fecha_de_Inicio == ymd(date),
           Sector == var) %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      ICP = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      IRP = sum(Total_de_Recipientes_Positivos)/
        sum(Total_de_Recipientes_con_Agua)*100,
      IB = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)

}


