#'  clean raw data from a entomology study
#'
#' @description
#' The function creates a data.frame that contains data and changes type data. The
#' data is assumed to have been previously loaded using the function
#' "Load_raw_data" from a .txt file associated with an entomology study
#' conducted on the platform "Vigilancia Entomológica y Control Integral del
#' Vector". This function aims to generate new data.frame suitable for
#' further analysis and manipulation. It also saves the data from the
#' data.frame to a .csv file
#'
#' @param df a data.frame with raw data path
#' @param path_out path for that cleaned data
#' @param col_name select variables
#'
#' @return
#'
#' Data.frame A .csv file with selected and cleaned data
#' @export
#'
#' @examples
#'

clean_raw_data <- function(
    df,
    path_out = "~/CursoQR/Package1/rStegomyiaIndices/data-raw/qr.csv",
    col_name = list(
      Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
      Clave_Jurisdiccion = "f",
      Jurisdiccion = "f",
      Clave_Municipio = "f",
      Municipio = "f",
      Clave_Localidad = "f",
      Localidad = "f",
      Sector = "f",
      Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
      Semana_Epidemiologica = "f",
      Casas_Revisadas = "d",
      Casas_Positivas = "d",
      Total_de_Recipientes_con_Agua = "d",
      Total_de_Recipientes_Positivos = "d"
    )
){
  write_csv(df, path_out)

  df_aux <- read_csv(
    path_out,
    col_types = col_name,
  )
  df <- df_aux
  write_csv(df, path_out)
  return(df)
  }
