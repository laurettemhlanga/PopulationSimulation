
#' wedge_excess_mortality_array
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param max_age the optimal age each birth cohort is reaches
#' @param list_of_birth_times a numeric vectors of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
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


wedge_excess_mortality_array <- function(max_age,
                                         list_of_birth_times,
                                         excess_mortality,
                                         time_step)
  {
  # populates an excess_mortality_array function based on the excess_mortality function supplied
  # the maximum age, list of times, timesinceinfection (tau) and the required time-step. Note the approximation
  # is for being infected in the interval in question is calculated at mid point.

  ages  <- seq(0, max_age, time_step)
  #taus <- seq(1, max_age, time_step)
  taus <- ages[-1]

  excess_mort_array <-  array(NA, dim = c(length(list_of_birth_times) + (length(ages)-1), length(ages), length(taus)))

  age_index <- 1:length(ages)
  birth_time_index <- 1:length(list_of_birth_times)
  tau_index <- 1:length(taus)

  for (age in age_index){
    for (tau in tau_index){

      if (ages[age] < taus[tau]){

        excess_mort_array[birth_time_index + (age -1), age, tau ] <- NA

      }else{

      excess_mort_array[birth_time_index + (age -1), age, tau ] =  excess_mortality( matrix_of_ages = ages[age] + 0.5 * time_step ,
                                                                      matrix_of_times = list_of_birth_times + (ages[age]+ 0.5 * time_step),
                                                                      value_of_tau = taus[tau] + 0.5 * time_step)
      }
    }
  }

  excess_mort_array_2 <- array(NA, dim = c((nrow(excess_mort_array) - ncol( excess_mort_array)) + 1,
                                                     dim( excess_mort_array)[2], dim( excess_mort_array)[3]))

  #loop to restructure the excess mortality into a rectangular shapes vs the parallegram shape from the output above.

  for (timesinceinfection in 1:dim(excess_mort_array)[3]){

    excess_mort_array_2[ , , timesinceinfection] <- compress_age_time_matrix(excess_mort_array[, , timesinceinfection])
  }


  return(excess_mort_array_2)
}








# y = excess_mortality_array(max_age = 3,
#                        list_of_birth_times = 1:5,
#                        excess_mortality = tau_linear_excess_mortality,
#                        time_step  = 1)
