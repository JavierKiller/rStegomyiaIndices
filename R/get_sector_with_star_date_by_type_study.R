#' Get sector with start date by type study
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with the function `load_raw_data` and cleaned using the function
#' `clean_raw_data`, you can select the study type and geographic variable using the
#' following formulas:
#' - Container Index (CI): (number of infected containers / total number of containers) * 100
#' - House Index (HI): (number of infected houses / total number of houses) * 100
#' - Breteau Index (BI): (number of positive containers / number of houses explored) * 100
#'
#' Compare the Stegomyia indices in a sector by type of study.
#'
#' @param df The dataframe with the information.
#' @param pathout Path to the output CSV file (e.g., "data-raw/table_indices_sector_in_date.csv").
#'
#' @return The dataframe with Stegomyia indices for the selected type of study and geographic variable.
#'
#' @examples
#' get_sector_with_start_date_by_type_study(df0, path_out = "data-raw/example_table_indices_sector_in_date.csv")
#'
#' @export

get_sector_with_star_date_by_type_study <- function(
  df,
  path_out = "data-raw/table_indices_sector_in_date.csv"
  ){
    if (nrow(df) == 0) {
      stop("These filters don't have data in this data.frame")
      }
    if (any(df$Casas_Revisadas == 0)) {
      warning("Casa_Revisada with 0")
    }
  df0 <- df %>%
    filter(Casas_Revisadas != 0)
  df0 <- df0 %>%
    dplyr::select(Tipo_de_Estudio,
                        Sector,
                        Fecha_de_Inicio,
                        Casas_Revisadas,
                        Casas_Positivas,
                        Total_de_Recipientes_con_Agua,
                        Total_de_Recipientes_Positivos)
 #df0

 df_e <- df0 %>%
    filter(Tipo_de_Estudio == "Encuesta")%>%
    rename(Fecha_de_Inicio_E = Fecha_de_Inicio,
            Casas_Revisadas_E = Casas_Revisadas,
            Casas_Positivas_E = Casas_Positivas,
             Total_de_Recipientes_con_Agua_E = Total_de_Recipientes_con_Agua,
             Total_de_Recipientes_Positivos_E = Total_de_Recipientes_Positivos)
 #df_e

 df_v <- df0 %>%
    filter(Tipo_de_Estudio == "Verificacion")%>%
    rename(Fecha_de_Inicio_V = Fecha_de_Inicio,
           Casas_Revisadas_V = Casas_Revisadas,
           Casas_Positivas_V = Casas_Positivas,
           Total_de_Recipientes_con_Agua_V = Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos_V = Total_de_Recipientes_Positivos)
 #df_v

 df_ev <- df_e %>%
   inner_join(df_v, by = "Sector")%>%
   mutate(sum_na =  rowSums(is.na(.)))
 null_count <- sum(df_ev$sum_na)
 print(null_count)

 df_evt <- df_ev %>%
   group_by(Sector) %>%
   summarize(
     #Days = (Fecha_de_Inicio_V - Fecha_de_Inicio_E),
     HI_E = sum(Casas_Positivas_E) / sum(Casas_Revisadas_E) * 100,
     HI_V = sum(Casas_Positivas_V) / sum(Casas_Revisadas_V) * 100,
     CI_E = if (sum(Total_de_Recipientes_Positivos_E) > 0) {
       sum(Total_de_Recipientes_Positivos_E) / sum(Total_de_Recipientes_con_Agua_E) * 100
     } else {
       0
     },
     CI_V = if (sum(Total_de_Recipientes_Positivos_V) > 0) {
       sum(Total_de_Recipientes_Positivos_V) / sum(Total_de_Recipientes_con_Agua_V) * 100
     } else {
       0
     },
     BI_E = sum(Total_de_Recipientes_Positivos_E) / sum(Casas_Revisadas_E) * 100,
     BI_V = sum(Total_de_Recipientes_Positivos_V) / sum(Casas_Revisadas_V) * 100
   ) %>%
   ungroup()
 df <- df_evt
 # write_csv(df_evt, file = path_out)
 return(df_evt)
  }
