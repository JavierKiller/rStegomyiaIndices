#'  clean raw data from a entomology study
#'
#' @description
#' The function creates a data.frame containing data and chance type data. The
#'  data is assumed to have been previously loaded using the function
#'  "Load_raw_data" from a .txt file associated with an entomology study
#'  conducted on the platform "Vigilancia Entomológica y Control Integral del
#'  Vector". This function aims to generate new data.frame suitable for
#'  further analysis and manipulation. I will also save the data from the
#'  data.frame to a .csv file
#'
#' @param `df` a data.frame with raw data path
#' @param `path_out` path for that cleaned data
#' @param `col_name` select variables
#'
#' @return Data.frame A .csv file with selected and cleaned data
#' @export
#'
#' @examples
#'
#' path <- "./data-raw/estudio_entomologico.txt"
#'
#' df <- load_raw_data(df)
#'
# TODO: camabiar la documentacion ####
clean_raw_data <- function(
    df,
    colt= list(
      Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
      Clave_Jurisdiccion = "f",
      Jurisdiccion = "f",
      Clave_Localidad = "f",
      Localidad = "f",
      Sector = "f",
      Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
      Semana_Epidemiologica = "f",
      Casas_Revisadas = "d",
      Casas_Positivas = "d",
      Total_de_Recipientes_con_Agua = "d",
      Total_de_Recipientes_Positivos = "d"
    ),
    path_out = "~/CursoQR/Package1/rStegomyiaIndices/rStegomyiaIndices/data-raw/qr.csv"
){

  write_csv(df, path_out)

  df_aux <- read_csv(
    path_out,
    col_types = colt,
  )
  df <- df_aux

  return(df)
}

