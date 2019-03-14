#' base_mortality_fun
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of mortality when
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param exmin numeric, indicates minimum mortality, which is at age_min
#' @param exfin numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export

#Option 1






time_indep_age_linear_base_mortality <- function(matrix_of_ages, matrix_of_times, constant = 0, age_min = 0,
                               age_max = 50, mort_min = 0, mort_max = 0.01)
  {
  age <- matrix_of_ages

  if (constant>0) {

    return(matrix(rep(constant, ncol(age) * nrow(age)),  ncol = ncol(age), nrow = nrow(age) ))

  } else {

    base_mortality = ifelse(matrix_of_ages <= age_min, 0,
                            ifelse(matrix_of_ages <= age_max, mort_min + ((mort_max - mort_min)/(age_max - age_min)) * (matrix_of_ages - age_min),
                                   0))

    return(base_mortality)
  }
}

#









# base_mortality_fun <- function(t, constant = 0.01, age_min = 1,
#                                     age_max = 50,
#                                     exmin =0,
#                                     exfin =0.01){
#
#   base_mortality = ifelse(t <= age_min, 0,
#                           ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
#                                  0))
#
#   return(base_mortality)
# }



# Option # 2
# base_mortality_fun <- function(t=seq(50,0.55), age = 0:49) {
#
#   base_mortality = exp(-(1/age)*t)*0.1
#
#   return(base_mortality)
# }
