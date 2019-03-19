#' incidence_matrix_fun
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#'
#' @param incidence_matrix an incidence matrix
#' @param base_mortality_matrix a mortality matrix
#' @param time_step the time step between consecurtive birth_dates
#' @return a matrix of the probabiity of getting infected at a given age and time.
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export




incidence_matrix_exp <- function(incidence_matrix,
                                 base_mortality_matrix,
                                 time_step)
{
  
  
  attrition_rate <-  (1 - susceptible_cumulative_survival(incidence_matrix,
                                                          base_mortality_matrix,
                                                          time_step))

  incidence_adjusted <-  (incidence_matrix/(incidence_matrix + base_mortality_matrix)) * attrition_rate[ ,-ncol(attrition_rate)]
   
  return(incidence_adjusted)

}


mod = incidence_matrix_exp(incidence_matrix = incidence_m,
                     base_mortality_matrix = base_mortality_m,
                     time_step = 1)
