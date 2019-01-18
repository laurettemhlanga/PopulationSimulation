# generate_mortality ?????????



#'function for base mortality 
#' @param t time .
#' @param conc constant mortality 
#' @param agemin minimum age 
#' @param exmin minimum excess mortality 
#' @param exmax maximum excess mortality 
#' @return returns a number or or a vector of mortality rates for a a given age and time given  \code{t},\code{conc}, \code{exmax},\code{exmin}, and \code{exfin}
#' @examples
#' generate_birth_counts(1000, 1984 : 1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)



generate_base_mortality <- function(t, conc = 0.01, agemin = 1,  
                                   agemax = 50,   
                                   exmin =0,  
                                   exfin =0.01){
  #varying mortality
  Ex_mort = ifelse(t <= agemin, 0, 
                   ifelse(t <= agemax, exmin + ((exfin - exmin)/(agemax - agemin)) * (t - agemin),
                          0))
  
  return(Ex_mort)
}

