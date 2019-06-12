#' time_indep_age_linear_excess_mortality
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param value_of_tau numeric, indicates a constant rate of mortality when
#' @param intercept numeric, indicates minimum age to be included in the simulation
#' @param slope numeric, indicates maximum age to be included in the simulation
#' @return a numeric vector that represents the mortality rate at t.
#'
#'
#'
#' @export

tau_linear_excess_mortality  <- function(matrix_of_ages, matrix_of_times, value_of_tau,
                                                    intercept = 0.05,
                                                    slope = 0.1)
{

  # calculates excess mortality as a function of age and  time since infection and ignores time (based on williams_2014). The excess
  # mortality resulting is a weibull function. Note if a non zero value is provided for the variable constant then a
  # constant excess mortality is obtatined i.e.  excess mortality (age, time, time since infection) = constant.

  tau <- value_of_tau
  ncols <- ncol(matrix_of_ages)
  nrows <- nrow(matrix_of_ages)
  # requires sensible thought

  if (is.matrix(matrix_of_ages) == T){

    ex_mort_tau <-  matrix(rep(intercept + (slope * tau) , ncols *nrows),
                         ncol = ncols,
                         nrow = nrows)
  }else{

    ex_mort_tau <- intercept + (slope * tau)}

  return(ex_mort_tau)
}



# tau_linear_excess_mortality(matrix_of_ages = 5,
#                             matrix_of_times = 4,
#                             value_of_tau = 2,
#                             intercept = 0.1,
#                             slope = 0.5)










#' survival_prob_infected_mahiane
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i umeric, indicates time since infection among the infected poplation
#' @param shape numeric, indicates minimum age to be included in the simulation
#' @param max_survival  numeric, indicates minimum mortality, which is at age_min
#' @param min_survival numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param age_min umeric, indicates minimum age to be included in the simulation
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export






excess_mahiane <- function(matrix_of_ages,
                                   matrix_of_times,
                                   times_since_i,
                                   shape = 2,
                                   max_survival = 16, min_survival = 6.6,
                                   age_max = 50, age_min = 0)
{

  scale <- max_survival -
    ((max_survival - min_survival)/(age_min - age_max)) * matrix_of_ages

  excess_mortality <-  (times_since_i / scale) ^ shape

  return(excess_mortality)

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




