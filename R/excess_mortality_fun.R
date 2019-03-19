#' excess_mortality_fun
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @param matrix_of_age numeric, indicates age
#' @param matrix_of_time numeric, indicates average time
#' @param matrix_of_time_since_i umeric, indicates time since infection among the infected poplation
#' @param constant numeric, indicates a constant rate of excess mortality
#' @param shape_parameter numeric, indicates weibull shape parameter
#' @param median_survival numeric, indicates median survival time of the infected individuals
#' @return a numeric vector that represents the excess mortality rate at age, time since infection and time.
#'
#' @export

excess_mortality_fun <- function(matrix_of_ages, matrix_of_time, time_since_i,
                                 constant = 0, shape_parameter = 2,
                                 median_survival = 10.6)
{

  # calculates excess mortality as a function of age and  time since infection and ignores time (based on williams_2014). The excess
  # mortality resulting is a weibull function. Note if a non zero value is provided for the variable constant then a
  # constant excess mortality is obtatined i.e.  excess mortality (age, time, time since infection) = constant.

  age <- matrix_of_ages
  # requires sensible thought


  if (constant > 0) {

    return(array(rep(constant, ncol(age) * nrow(age) * ncol(age)),  dimn = c(nrow(age), ncol(age), ncol(age))))

  }else{

     Ex_mort_tau <-   2 ^ (-((age - time_since_i) / median_survival) ^ shape_parameter)

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






