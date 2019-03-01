#' incidence_matrix_fun
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#'
#' @param incidence_matrix an incidence matrix
#' @param base_mortality_matrix a mortality matrix
#' @param delta the time step between consecurtive birth_dates
#' @return a matrix of the probabiity of getting infected at a given age and time.
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export




incidence_matrix_exp <- function(incidence_matrix,
                                 base_mortality_matrix,
                                 delta)
{

  incidencce_adjusted <-  transform_data(incidence_matrix/(incidence_matrix + base_mortality_matrix)) *
    (1 - susceptible_cumulative_survival_fun(incidence_matrix,
                                             base_mortality_matrix,
                                             delta))
  return(incidencce_adjusted)

}
