#'probability_surviving_array
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param max_age denotes the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param list_of_times a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param time_step the time or age difference between to consecutive times or ages i.e it is uniform in all values supplied
#' @param base_mortality a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @param excess_mortality a function which takes as arguments age, time and tau - i.e. the time since infection among the infected population - and returns a numberic rate of mortality for each age and time included in the simulation
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @return returns an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#'


probability_surviving_infected <- function(max_age,
                                           list_of_times,
                                           time_step,
                                           excess_mortality,
                                           base_mortality)
{
  # calculates the probability of surviving in the infected state after infection at time
  # t - tau, age - tau  for a duration of tau years. Based on the excess and base mortality function an array of the respective survival probabilities
  # for a given age, time and  time since infection is created.

  times  <- seq(min(list_of_times), max(list_of_times), time_step)
  ages <- seq(0, max_age, time_step)
  times_since_i <- seq(0, max_age, time_step)

  probability_surviving_array <-  array(NA, dim = c(length(times) + max_age, length(ages), length(ages)))


  for (aa in seq_along(ages)){
    for (ta in seq_along(times_since_i)){

      if (ages[aa] < times_since_i[ta]){

        probability_surviving_array[seq_along(times) + (aa - 1), aa, ta] <- 0


      }

      else{

        probability_surviving_array[seq_along(times) + (aa - 1), aa, ta] <- ((base_mortality((times + (ages[aa] + (0.5 * time_step))), ages[aa] + ((0.5 * time_step))) +
                                                                                excess_mortality((matrix_of_times = times + (ages[aa] + (0.5 * time_step)))- times_since_i[ta],
                                                                                                 matrix_of_ages = (ages[aa] + (0.5 * time_step)) - times_since_i[ta],
                                                                                                 times_since_i = times_since_i[ta])) * time_step)


        #exp(- 0.01)

      }

    }
  }



  probability_surviving_array_2 <- array(NA, dim = c((nrow(probability_surviving_array) - ncol(probability_surviving_array)) + 1,
                                                     dim(probability_surviving_array)[2], dim(probability_surviving_array)[3]))

  for (ta in 1:dim(probability_surviving_array)[3]){

    probability_surviving_array_2[ , , ta] <- transform_data(probability_surviving_array[, , ta])
  }

  return(probability_surviving_array_2)

}



# y = probability_surviving_infected(max_age = 3,
#                                   list_of_times = 1:5,
#                                   time_step = 1,
#                                   excess_mortality = time_indep_age_linear_excess_mortality,
#                                   base_mortality = time_indep_age_linear_base_mortality)

