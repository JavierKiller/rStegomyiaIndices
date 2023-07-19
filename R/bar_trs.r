#' Make graphic bar of calculate proportion of type infected containers
#'
#'
#' @param `df` the dataframe with information
#' @param `var`  The variable used to calculate proportion of type infected
#'   containers and make graphic bar.
#' @param `path_out`  path for that graphic bar. By default, it is
#'   "./data/g_bar.jpg"
#'
#' @return
#' make bar graphric with percentages of the typology infect container by
#' select  variable
#'
#' @examples
#' bar_trs(df, "Tipo_de_Estudio", "typeofstudy.jpg")
#'
#' @export

bar_trs <- function(df, var, file_name="./data/g_bar.jpg") {

  dfts <- df %>%
    filter(Recipientes_Tratables != 0 |
             Recipientes_Controlables != 0 |
             Recipientes_Eliminables !=0) %>%
    group_by(Tipo_de_Estudio, !!sym(var)) %>%
    summarise(Recipientes_Tratables,  Recipientes_Controlables,
              Recipientes_Eliminables) %>%
    mutate(Pct_Recipientes_Tratables = Recipientes_Tratables/
             (Recipientes_Tratables + Recipientes_Controlables +
                Recipientes_Eliminables)*100,
           Pct_Recipientes_Controlables = Recipientes_Controlables/ (
             Recipientes_Tratables + Recipientes_Controlables +
               Recipientes_Eliminables)*100,
           Pct_Recipientes_Eliminables = Recipientes_Eliminables/ (
             Recipientes_Tratables + Recipientes_Controlables +
               Recipientes_Eliminables)*100)


  bar_tr_plot <- ggplot(data = dfts) +
    geom_bar(
      aes(x = "", y = Pct_Recipientes_Tratables, fill = Tipo_de_Estudio),
      stat = "identity",
      position = "dodge",
      width = 0.2) +
    geom_bar(
      aes(x = "1", y = Pct_Recipientes_Controlables, fill = Tipo_de_Estudio),
      stat = "identity",
      position = "dodge",
      width = 0.2) +
    geom_bar(
      aes(x = "2", y = Pct_Recipientes_Eliminables, fill = Tipo_de_Estudio),
      stat = "identity",
      position = "dodge",
      width = 0.2) +
    scale_x_discrete(labels = c("Recipientes
                                Tratables",
                                "Recipientes
                                Controlables",
                                "Recipientes
                                Eliminables"),
                     limits = c("", "1", "2")) +
    labs(x = "", y = "Porcentaje", fill = "Tipo de Estudio") +
    facet_wrap(as.name(var), ncol = 3)
  print(bar_tr_plot)

  ggsave(file_name, plot = bar_tr_plot, width = 10, height = 8, dpi = 300)
  return(bar_tr_plot)
}
