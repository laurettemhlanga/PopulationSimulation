#' incidence_fun
#'
#' a function that takes as arguments age and time and returns a numeric vector of length 1
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of incidence
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param age_peak numberic, indicates age at which indicence reaches maximum, which is at Ipeak
#' @param Imin numeric, indicates minimum incidence, which is at age_min
#' @param Ipeak numeric, indicates maximum incidence at Ipeak
#' @param Ifin numeric, indicates incidence at Ipeak
#' @return a numeric vector that represents the incidence rate at age and time.
#'
#' @export


time_indept_age_tent_incidence <- function(matrix_of_ages, matrix_of_times, constant = 0, age_min = 0,
                               age_max = 50,  age_peak= 25,
                               Imin =0.01,  Ipeak =0.05,
                               Ifin =0.02)
{
  age <- matrix_of_ages

  if (constant>0) {

    return(matrix(rep(constant,ncol(age)*nrow(age)),  ncol = ncol(age), nrow = nrow(age) ))

  } else {

    incidence = ifelse(age <= age_min, 0,
                       ifelse(age <= age_peak, Imin + ((Ipeak - Imin)/(age_peak - age_min)) * (age - age_min),
                              ifelse(age <= age_max, Ipeak + ((Ifin - Ipeak )/(age_max - age_peak)) * (age - age_peak), 0)))

    return(incidence)
  }
}





# Option 1
# tent_incidence_fun <- function(age, constant = 0, age_min = 0,
#                                age_max = 50,  age_peak= 25,
#                                Imin =0.01,  Ipeak =0.05,
#                                Ifin =0.02)
# {
#
#   if (constant>0) {
#     return(matrix(rep(constant,ncol(age)*nrow(age)),  ncol = ncol(age), nrow = nrow(age) ))
#   } else {
#   incidence = ifelse(age <= age_min, 0,
#                      ifelse(age <= age_peak, Imin + ((Ipeak - Imin)/(age_peak - age_min)) * (age - age_min),
#                             ifelse(age <= age_max, Ipeak + ((Ifin - Ipeak )/(age_max - age_peak)) * (age - age_peak), 0)))
#
#   return(incidence)
#   }
# }


