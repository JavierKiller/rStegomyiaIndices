#' Load raw data from a entomology study
#'
#'
#' @param `path` a string with raw data path
#' @param `col_name` select variables
#'
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
    col_select = col_name,
    locale = locale(encoding = "UTF-16" )
  )
  # Eliminate spaces in the column names and replace them with underscores _
  colnames(df) <-
    str_replace_all(colnames(df), pattern = " ", replacement = "_")
  # Create four new columns by separating the two variable from the original
  # columns
  df <- df %>%
    separate(Localidad, into = c("Clave_Localidad", "Localidad"),
             sep = " ")
  df <- df %>%
    separate(Jurisdiccion, into = c("Clave_Jurisdiccion", "Jurisdiccion"),
             sep = " ")


 return(df)
}
