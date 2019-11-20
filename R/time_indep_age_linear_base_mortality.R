#' time_indep_age_linear_base_mortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of mortality when
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param mort_min  numeric, indicates minimum mortality, which is at age_min
#' @param mort_max numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export

#Option 1






time_indep_age_linear_base_mortality <- function(ages, times, constant = 0, age_min = 0,
                               age_max = 50, mort_min = 0, mort_max = 0.01)
  {

  # calculates base mortality rate as a function of age and is constant in time. The base mortality resulting is a
  # linear function of age. Note if a non zero value is provided for the variable constant then a
  # constant base mortality rate is obtatined i.e.  base mortality (age, time) = constant.

  age <- ages

  if (constant > 0) {

    return(matrix(rep(constant, ncol(age) * nrow(age)),  ncol = ncol(age), nrow = nrow(age)))

  } else {

    base_mortality = ifelse(age <= age_min, 0,
                            ifelse(age <= age_max,
                                   mort_min + ((mort_max - mort_min)/(age_max - age_min)) * (age - age_min), 0))

    return(base_mortality)
  }
}

###################################

#' step_mortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export

#Option 1




step_mortality <- function(ages,
                           times){

  mortality =   0.01#0.01 + 0.001 * ages

  return(mortality)
}





###################################

#' no_basemortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export

#Option 1




no_basemortality <- function(ages,
                           times){

  mortality =  rep( 0, length(ages))

  return(mortality)
}

