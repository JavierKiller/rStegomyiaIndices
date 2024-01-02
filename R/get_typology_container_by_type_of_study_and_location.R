#' Positive Typology Container by type of study and location
#'
#' Complementary to the Stegomyia indices, the type of positive container is
#'  determined to establish the preference of mosquitoes in the use of
#'  breeding sites.
#'
#' @param df the dataframe with information.
#' @param st The type of study selected. By default, it is set to
#' "Verificacion".
#' @param var The location with geographic variable used to .
#'
#' @return
#'
#' The dataframe with location of the selection of type of study and
#' geographic variable.
#'
#' @export
#'
#' @examples
#' get_typology_container_by_type_of_study_and_location(df,
#'  st = "Verificacion",
#'  var ="CIUDAD_OBREGÓN").
#'
####TODO: actualizar documentation####
get_typology_container_by_type_of_study_and_location <- function(df,
                 st = "Verificacion",
                 var = c("CIUDAD_OBREGÓN","HERMOSILLO"))
{
  dft <- df %>%
    filter(Localidad %in% var,
           Tipo_de_Estudio %in%  st)
  condiction <- nrow(df %>%
                       filter(Casas_Revisadas == 0))
  if (condiction !=0){
    warning("Casa_Revisada with 0")
    #print(condiction)
    dft <- dft %>%
      filter(Casas_Revisadas != 0)
  }
  dfts <- dft %>%
    filter(Total_de_Recipientes_Positivos > 0
    ) %>%
    dplyr::select(Tipo_de_Estudio,
           Localidad,
           Recipientes_Tratables,
           Recipientes_Controlables,
           Recipientes_Eliminables,
           Total_de_Recipientes_Positivos) %>%
    group_by( Localidad ) %>%
    summarize(
      Pct_Recipientes_Tratables =
        sum(Recipientes_Tratables) /
        sum(Total_de_Recipientes_Positivos) * 100,
      Pct_Recipientes_Controlables =
        sum(Recipientes_Controlables) /
        sum(Total_de_Recipientes_Positivos) * 100,
      Pct_Recipientes_Eliminables =
        sum(Recipientes_Eliminables) /
        sum(Total_de_Recipientes_Positivos) * 100
    )
  return(dfts)
}


# df0 <- get_typology_container_by_type_of_study_and_location(df = df,
#                                                      st = "Encuesta",
#                                                      var = c("HERMOSILLO","NAVOJOA", "CIUDAD_OBREGÓN")
# )
# df0
