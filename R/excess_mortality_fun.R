#' excess_mortality_fun
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @param matrix_of_age numeric, indicates time or times at which the excess mortality rate is desired
#' @param matrix_of_time numeric, indicates average time since infection among the infected poplation
#' @param constant numeric, indicates a constant rate of excess mortality
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param mort_min numeric, indicates minimum excess mortality, which is at age_min
#' @param mort_max numeric, indicates maximum/final excess mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the excess mortality rate at age and time.
#'
#' @export

excess_mortality_fun <- function(matrix_of_age, matrix_of_time, matrix_of_time_since_i, constant = 0, age_min = 0,
                                 age_max = 50,
                                 mort_min =0.01,
                                 mort_max = 0.05)
{

  age <- matrix_of_ages
  time_since_i <- matrix_of_time_since_i

  if (constant > 0) {

    return(matrix(rep(constant,ncol(age) * nrow(age)),  ncol = ncol(age), nrow = nrow(age)))

  }else{

  Ex_mort_tau = ifelse(age <= age_min, 0,
                       ifelse(age <= age_max, mort_min + ((mort_max - mort_min)/(age_max - age_min)) * ta *(age - age_min),
                              0))
  }
  return(Ex_mort_tau)
}


