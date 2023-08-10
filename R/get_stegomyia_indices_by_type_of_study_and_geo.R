#' Get stegomyia indices by type of study and geo
#'
#'  @description
#'  The Stegomyia indices are calculated for each sampling select study type
#'  and Sector geographic variable using the following formulas:
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
#'    indices.
#'  @return
#'  The dataframe with stegomyia indices of the selection of type of study and
#'    geographic variable.
#' @export
#'  @examples
#'  get_stegomyia_indices_by_type_of_study_and_geo(df, "390",
#'   "Localidad").
#'
get_stegomyia_indices_by_type_of_study_and_geo <- function(
      df,
      st ="Verificacion",
      var
)
{
   filtered_df <- df %>%
      filter(Tipo_de_Estudio == st, Sector == var)
   condicion_nrows <- nrow(filtered_df)>0
   if (isFALSE(condicion_nrows)){
      stop("These filters don´t have data in this data.frame")
   }
   dfti <- filtered_df %>%
      filter(Tipo_de_Estudio ==  st, Sector == var) %>%
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
