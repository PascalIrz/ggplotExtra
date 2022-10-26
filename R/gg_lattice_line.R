####################################
#' Lattice diagram with ggplot with display options
#'
#' @param df Dataframe with the data. Must contain the variables described below.
#' @param var_x,var_y,var_lattice Variable for the x axis, y axis, and lattice wrapping.
#' @param threshold_x Numerical. If given, the line left from the threshold is dotted.
#'     Typically used to distinguish time periods of distinct data reliability.
#' @param display_mean Boolean. Horizontal line for y mean value?
#' @param free_y Boolean. If TRUE, the y axis scale can vary across elementary plots.
#'     If FALSE (default), the y axis scale is identical across elementary plots.
#' @param lab_x,lab_y String. Axis labels.
#'
#' @return The ggplot lattice diagram.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_y_continuous guides labs geom_segment
#' @importFrom dplyr pull filter group_by summarise enquo
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' # install / load packages
#' install_github("PascalIrz/aspe")
#' library(aspe)
#' library(tidyverse)
#'
#' # load data (can be downloaded from https://zenodo.org/record/7099129)
#' load(file = "tables_aspe.RData")
#'
#' # create appropiiate dataframe
#' my_station <- 10734
#'
#' my_surveys <- mef_creer_passerelle() %>%
#'   filter(sta_id == my_station) %>%
#'   mef_ajouter_ope_date() %>%
#'   select(ope_id,
#'          annee) %>%
#'   distinct() %>%
#'   filter(annee > 1999)
#'
#' my_data <- operation_description_peche %>%
#'   inner_join(my_surveys, by = c("odp_ope_id" = "ope_id")) %>%
#'   select(annee,
#'          odp_duree_peche,
#'          odp_longueur,
#'          odp_largeur_lame_eau,
#'          odp_temperature_instantanee,
#'          odp_conductivite,
#'          odp_tension,
#'          odp_intensite,
#'          odp_puissance) %>%
#'   pivot_longer(odp_duree_peche:odp_puissance,
#'                names_to = "variable",
#'                values_to = "value")
#'
#' # plot the diagram
#' gg_lattice_line(df = my_data,
#'                 var_x = annee,
#'                 var_y = value,
#'                 var_lattice = variable,
#'                 threshold_x = 2010,
#'                 free_y = TRUE,
#'                 lab_x = "",
#'                 lab_y = "")
#' }
gg_lattice_line <- function(df,
                            var_x,
                            var_y,
                            var_lattice,
                            threshold_x = NULL,
                            display_mean = TRUE,
                            free_y = FALSE,
                            lab_x,
                            lab_y)
{
  # managing lazy eval
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_lattice <- enquo(var_lattice)

  # removing missing values
  df <- na.omit(df,
                cols = !!var_y)

  # in th eabsence of threshold_x, set it to x min value
  if (is.null(threshold_x))
  {
    threshold_x <- df %>%
      pull(!!var_x) %>%
      min()
  }

  # split df into df1 and df2, according to threshold_x
  df1 <- df %>%
    filter(!!var_x <= threshold_x)

  df2 <- df %>%
    filter(!!var_x >= threshold_x)

  # pre-process means. If not, all displayed identical on diagram
  means <- df %>%
    filter(!!var_x >= threshold_x) %>%
    group_by(!!var_lattice) %>%
    summarise(means = mean(!!var_y, na.rm = TRUE))

  # max_x for the geom_segment
  max_x <- df %>%
    pull(!!var_x) %>%
    max()

  # plot
  g <- ggplot(data = df,
              aes(x = !!var_x,
                  y = !!var_y)) +
    geom_line(data = df1,
              aes(x = !!var_x,
                  y = !!var_y),
              linetype = "dotted") +
    geom_line(data = df2,
              aes(x = !!var_x,
                  y = !!var_y)) +
    guides(color = "none") +
    labs(x = lab_x,
         y = lab_y) +
    scale_y_continuous(limits = c(0, NA))

  # if free_y
  if(!free_y)
  {g <- g + facet_wrap(facets = var_lattice)
  } else {
    g <- g + facet_wrap(facets = var_lattice,
                        scales = "free_y")
  }

  # if display_mean
  if (display_mean)
  {
    g <- g + geom_segment(
      data = means,
      aes(
        x = threshold_x,
        y = means,
        xend = max_x,
        yend = means
      ),
      col = "red"
    )
  }
  g
}

