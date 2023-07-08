#' Load and clean raw data from a entomology study
#'
#'
#' @param path a string with raw data path
#' @param path_out path for that cleaned data
#' @param col_name select variables
#'
#' @return A .csv file with selected and cleaned data
#' @export
#'
#' @examples
#'
#' path <- "./data-raw/estudio_entomologico.txt"
#'
#' col_select1 <- c("Tipo de Estudio",
#'                  "Jurisdiccion", "Localidad",
#'                  "Sector", "Fecha de Inicio",
#'                  "Semana Epidemiologica",
#'                  "Casas Revisadas",
#'                  "Casas Positivas",
#'                  "Total de Recipientes con Agua",
#'                  "Total de Recipientes Positivos")
#'
#' df <- load_raw_data(path, col_name = col_select1)
#'
clean_raw_data <- function(
    path,
    path_out = "./data/qr.csv",
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
  dftr <- read_tsv(
    path,
    col_select = col_name,
    locale = locale(encoding = "UTF-16" )
  )

  # Eliminar los espacios en el nombre de las columnas y remplasarlos por _
  colnames(dftr) <-
    str_replace_all(colnames(dftr), pattern = " ", replacement = "_")
  # Crear dos nuevas columnas separando la variable de la columna original por un guión
  dftr <- dftr %>%
    separate(Localidad, into = c("clave_Localidad", "Localidad"), sep = " ")

  colt = list(
    Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
    Jurisdiccion = "f",
    clave_Localidad = "f",
    Localidad = "f",
    Sector = "f",
    Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
    Semana_Epidemiologica = "f",
    Casas_Revisadas = "d",
    Casas_Positivas = "d",
    Total_de_Recipientes_con_Agua = "d",
    Total_de_Recipientes_Positivos = "d"

  )



  write_csv(dftr, path_out)



  dftr1 <- read_csv(
    path_out,
    col_types = colt,
  )
  write_csv(dftr1, path_out)

  return(dftr1)
}

