#' time_indep_age_linear_excess_mortality
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of mortality when
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param mort_min  numeric, indicates minimum mortality, which is at age_min
#' @param mort_max numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the mortality rate at t.
#' @param times_since_i umeric, indicates time since infection among the infected poplation
#'
#'
#' @export

time_indep_age_linear_excess_mortality  <- function(matrix_of_ages, matrix_of_times,
                                                    constant = 0, age_min = 0, times_since_i,
                                                    age_max = 50, mort_min = 0.01,
                                                    mort_max = 0.05)
{

  # calculates excess mortality as a function of age and  time since infection and ignores time (based on williams_2014). The excess
  # mortality resulting is a weibull function. Note if a non zero value is provided for the variable constant then a
  # constant excess mortality is obtatined i.e.  excess mortality (age, time, time since infection) = constant.

  age <- matrix_of_ages

  times <- matrix_of_times
  # requires sensible thought


  if (constant > 0) {

    return(array(rep(constant, ncol(age) * nrow(age) * ncol(age)),  dim = c(nrow(age), ncol(age), ncol(age))))

  }else{

    Ex_mort_tau <-   (ifelse(matrix_of_ages <= age_min, 0,
                             ifelse(matrix_of_ages <= age_max,
                                    (mort_min + ((mort_max - mort_min)/(age_max - age_min)) * (matrix_of_ages - age_min)) * times_since_i, 0)))

    # multiplying by time since infection not meaning full think of a better time since  excess mortality function
  }

  return(Ex_mort_tau)
}
















#option A

# excess_mortality_fun <- function(matrix_of_age, matrix_of_time, matrix_of_time_since_i, constant = 0, age_min = 0,
#                                  age_max = 50,
#                                  mort_min =0.01,
#                                  mort_max = 0.05)
# {
#
#   # calculates excess mortality as a function of age and is constant in time. The excess mortality resulting is a
#   # linear function. Note if a non zero value is provided for the variable constant then a
#   # constant incidence is obtatined i.e.  base mortality (age, time) = constant.
#
#   age <- matrix_of_ages
#   time_since_i <- matrix_of_time_since_i
#
#   if (constant > 0) {
#
#     return(matrix(rep(constant,ncol(age) * nrow(age)),  ncol = ncol(age), nrow = nrow(age)))
#
#   }else{
#
#     Ex_mort_tau = ifelse(age <= age_min, 0,
#                          ifelse(age <= age_max, mort_min + ((mort_max - mort_min)/(age_max - age_min)) * ta *(age - age_min),
#                                 0))
#   }
#   return(Ex_mort_tau)
# }




