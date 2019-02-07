#' incidence_fun
#'
#' a function that takes as arguments age and time and returns a numeric vector of length 1
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param t numeric, indicates time or times at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of incidence
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param age_peak numberic, indicates age at which indicence reaches maximum, which is at Ipeak
#' @param Imin numeric, indicates minimum incidence, which is at age_min
#' @param Ipeak numeric, indicates maximum incidence at Ipeak
#' @param Ifin numeric, indicates incidence at Ipeak
#' @return a numeric vector that represents the incidence rate at age and time.

# Option 1
incidence_fun <- function(t, constant = 0.05, age_min = 0,
                               age_max = 50,  age_peak= 25,
                               Imin =0.01,  Ipeak =0.05,
                               Ifin =0.02)
{

  incidence = ifelse(t <= age_min, 0,
                     ifelse(t <= age_peak, Imin + ((Ipeak - Imin)/(age_peak - age_min)) * (t - age_min),
                            ifelse(t <= age_max, Ipeak + ((Ifin - Ipeak )/(age_max - age_peak)) * (t - age_peak), 0)))

  return(incidence)
}

