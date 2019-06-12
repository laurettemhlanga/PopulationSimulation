#'probability_surviving_array
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param max_age denotes the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param list_of_birth_times a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
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
                                           list_of_birth_times,
                                           time_step,
                                           excess_mortality,
                                           base_mortality)
{
  # calculates the probability of surviving in the infected state at time
  # tt , age  age for a duration of tau years. Based on the excess and base mortality
  # function an array of the respective survival probabilities
  # for a given age, time and  time since infection is created.

  birth_times  <- seq(min(list_of_birth_times), max(list_of_birth_times), time_step)
  ages <- seq(0, max_age, time_step)
  times_since_i <- seq(0, max_age, time_step)[-1]

  probability_surviving_array <-  array(NA, dim = c(length(birth_times) + max_age,
                                                    length(ages),
                                                    length(ages)-1))


  for (age in seq_along(ages)){
    for (timesinceinfection in seq_along(times_since_i)){

      if (ages[age] < times_since_i[timesinceinfection]){
        # remove possibility of assigning values to time since infections greater than the age as
        # it is not possible
        # seq_along gives the index of each element in the vector

        probability_surviving_array[seq_along(birth_times) + (age - 1), age, timesinceinfection] <- NA

      }

      else{

        probability_surviving_array[seq_along(birth_times) + (age - 1), age, timesinceinfection] <- exp(-((base_mortality((matrix_of_times = birth_times + (ages[age] + (0.5 * time_step))),
                                                                                             matrix_of_ages = ages[age] + ((0.5 * time_step))) +
                                                                                excess_mortality((matrix_of_times = birth_times + (ages[age] + (0.5 * time_step))),
                                                                                                 matrix_of_ages = (ages[age] + (0.5 * time_step)),
                                                                                                 times_since_i = times_since_i[timesinceinfection])) * time_step))
        #exp(- 0.01)
#
      }

    }
  }



  probability_surviving_array_2 <- array(NA, dim = c((nrow(probability_surviving_array) - ncol(probability_surviving_array)) + 1,
                                                     dim(probability_surviving_array)[2], dim(probability_surviving_array)[3]))

  for (timesinceinfection in 1:dim(probability_surviving_array)[3]){

    probability_surviving_array_2[ , , timesinceinfection] <- compress_age_time_matrix(probability_surviving_array[, , timesinceinfection])
  }

  return(probability_surviving_array_2)

}



# y = probability_surviving_infected(max_age = 3,
#                                   list_of_birth_times = 1:5,
#                                   time_step = 1,
#                                   excess_mortality = excess_mahiane,
#                                   base_mortality = time_indep_age_linear_base_mortality)

