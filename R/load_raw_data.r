#' Load raw data  of entomology study
#'
#' @description
#' Load_raw_data from a .txt file of the entomology study conducted on the
#' platform "Vigilancia Entomológica y Control Integral del Vector." Choose the
#' variables that will be used to calculate entomological risk indices. Fix
#' variable labels and eliminate spaces in the column names, replacing them
#' with underscores (_).If one of the columns labeled as Locality,
#' Municipality, or Jurisdiction is selected, two columns will be created from
#' the original one. the names of the new columns will be the key number of
#' the variable and the other with the name of the variable.
#'
#' @param `path` a string with raw data path
#' @param `col_name` names of variables,
#',
#' @return A data frame with selected and format in variable data
#' @export
#'
#' @examples
#'
#' path <- "./data-raw/estudio_entomologico.txt"
#'
#' col_select <- c("Tipo de Estudio",
#'                  "Jurisdiccion", "Localidad",
#'                  "Sector", "Fecha de Inicio",
#'                  "Semana Epidemiologica",
#'                  "Casas Revisadas",
#'                  "Casas Positivas",
#'                  "Total de Recipientes con Agua",
#'                  "Total de Recipientes Positivos")
#'
#' df <- load_raw_data(path, col_name = col_select)
#'

load_raw_data <- function(
    path,
    col_name = c(
      "Tipo de Estudio",
      "Jurisdiccion",
      "Localidad",
      "Sector",
      "Fecha de Inicio",
      "Semana Epidemiologica",
      "Casas Revisadas",
      "Casas Positivas",
      "Total de Recipientes con Agua" ,
      "Total de Recipientes Positivos"
    )
){
  df <- read_tsv(
    path,
    locale = locale(encoding = "UTF-16" )
  )
  names_df <- names(df)
  condition_tipo_de_estudio_error <-  "Tipo de Estudio" %in% names_df
  if (isFALSE(condition_tipo_de_estudio_error)) {
    stop("data file .txt dont have Tipo de Estudio o path is incorrect")
  }
  condicion_nrows <- nrow(df)>0
  if(isFALSE(condicion_nrows)){
    stop("data file .txt is empty")
  }
  df <- read_tsv(
    path,
    col_select = col_name,
    locale = locale(encoding = "UTF-16" )
  )
  # Eliminate spaces in the column names and replace them with underscores _
  colnames(df) <-
    str_replace_all(colnames(df), pattern = " ", replacement = "_")
  # Create four new columns by separating the two variable from the original
  # columns
  if("Localidad" %in% colnames(df)){
  df <- df %>%
    separate(Localidad,
             into = c("Clave_Localidad", "Localidad"),
             sep = " "
    )
  }
  if("Municipio" %in% colnames(df)){
  df <- df %>%
    separate(Municipio,
             into = c("Clave_Municipio", "Municipio"),
             sep = " "
    )
  }
  if("Jurisdiccion" %in% colnames(df)){
    df <- df %>%
      separate(Jurisdiccion,
               into = c("Clave_Jurisdiccion", "Jurisdiccion"),
               sep = " "
      )
  }
  return(df)
}
