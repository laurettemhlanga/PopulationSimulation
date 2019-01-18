#' generate_incidence is a function that takes as arguments age and time and returns a numeric vector representing a rate of incidence at the indicated time 
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package 
#' 
#' @param t numeric, indicates time at which the incidence rate is desired
#' @param conc ?
#' @param age_min numeric, indicates desired minimum age 
#' @param age_max numeric, indicates desired maximum age 
#' @param age_peak numberic, indicates age at which indicence reaches maximum, which is at \code{Ipeak}
#' @param Imin numeric, indicates minimum incidence, which is at \code{age_min}
#' @param Ipeak numeric, indicates maximum incidence at \code{Ipeak}
#' @param Ifin numeric, indicates incidence at \code{Ipeak}
#' @return a numeric vector that represents the incidence rate at \code{t}.
#' @examples To be entered
#' 

generate_incidence <- function(t, conc, age_min,  
                            age_max, age_peak, 
                            Imin, Ipeak, Ifin){
  #varying Inicdence (toy function)
  
  incidence = ifelse(t <= age_min, 0, 
                     ifelse(t <= age_peak, Imin + ((Ipeak - Imin)/(age_peak - age_min)) * (t - age_min),
                            ifelse(t <= age_max, Ipeak + ((Ifin - Ipeak )/(age_max - age_peak)) * (t - age_peak), 0)))
  
  return(incidence)
}

# Create .Rd file
devtools::document()


# Example, to be included witing roxygen code above
generate_incidence (t=7, conc = 0.05, age_min = 0,  
  age_max = 50,  age_peak= 25, 
  Imin =0.01,  Ipeak =0.05, 
  Ifin =0.02)
