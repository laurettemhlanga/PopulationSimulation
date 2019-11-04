
#' wedge_excess_mortality_matrix
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param n_age_steps the optimal age each birth cohort is reaches
#' @param list_of_times a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param excess_mortality the excess mortality function
#' @param time_step the time step between consecurtivelist_of_birth_times
#' @return returns an array of dimensions time, age and time since infection - tau, which is essentially the probability of surviving in the given time space.
#'
#' @export
#'
#'
#


#we need to agree values arising from calculations resulting from a < tau,
#at the moment we using an if statement to avoid such calculations.




wedge_excess_mortality_matrix <- function(n_age_steps, list_of_times, time_step,
                             excess_mortality
){

  ages <- seq(from = time_step / 2, by = time_step, length.out = n_age_steps)
  #ages  <- seq(time_step, max_age, time_step)
  taus  <- ages

  excess_mort <- matrix(NA, ncol = length(ages), nrow = length(taus))


  for (time_s in seq_along(taus)){

    excess_mort[time_s, ] <- ifelse(taus[time_s] <= ages ,
                                    excess_mortality(ages =  ages ,
                                                     times =  list_of_times ,
                                                     times_since_i = taus[time_s] ), NA)

  }

  return(excess_mort)
}


