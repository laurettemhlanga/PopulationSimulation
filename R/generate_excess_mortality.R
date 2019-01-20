#' a function that takes as arguments age and time and returns a numeric rate of mortality 
#' The \code{generate_excess_mortality} function is required as an argument for the packages main \code{do_sim} function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package 
#' 
#' @param t numeric, indicates time at which mortality rate is desired
#' @param conc ?
#' @param age_min numeric, indicates desired minimum age 
#' @param age_max numeric, indicates desired maximum age 
#' @param exmin numeric, indicates minimum rate of mortality, which is reached at \code{age_min}
#' @param exfin numeric, indicates peak mortality, which is reached at \code{age_max}
#' @return a numeric vector that represents the mortalit rate at \code{t}
#' @examples To be entered
#' 

generate_excess_mortality <- function(t, conc, age_min, age_max, exmin, exfin)
  {
  excess_mortality = ifelse(t <= age_min, 0, 
                   ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                          0))
  
  return(excess_mortality)
}

# Create .Rd file
devtools::document()





# example; to be entered above within roxygen code 

x <- generate_excess_mortality(t=7, conc = 0.05, age_min = 0,  
         age_max = 50,   
         exmin =0.01,  
         exfin =0.05)
View(x)
