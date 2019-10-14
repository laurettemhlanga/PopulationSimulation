#' time_indept_age_tent_incidence
#'
#' a function that takes as arguments age and time and returns a numeric vector of length 1
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param vector_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param vector_of_ages  numeric, indicates age or ages at which the incidence rate is desired
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


time_indept_age_tent_incidence <- function(vector_of_ages, vector_of_times, constant = 0, age_min = 0.01,
                               age_max = 50,  age_peak= 25,
                               Imin =0.01,  Ipeak =0.05,
                               Ifin =0.02)
{
  # calculates incidence rate as a function of age and time constant. The incidence rate distribution
  # has a tent/triangular function, with the tip of the tent at age_peak. Note if a non zero value is provided
  # for the variable constant then a constant incidence is obtatined i.e. incidence(age, time) = constant.


  age <- vector_of_ages

  if (constant > 0) {

    return(matrix(rep(constant,ncol(age)*nrow(age)),  ncol = ncol(age), nrow = nrow(age) ))

  } else {

    incidence = ifelse(age <= age_min, 0,
                       ifelse(age <= age_peak, Imin + ((Ipeak - Imin)/(age_peak - age_min)) * (age - age_min),
                              ifelse(age <= age_max, Ipeak + ((Ifin - Ipeak )/(age_max - age_peak)) * (age - age_peak), 0)))

    return(incidence)
  }
}







#' incidence_mahiane
#'
#' a function that takes as arguments age and time and returns a numeric vector of length 1
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param age_debut age at which individuals get HIV infected
#' @param beta shape parameter for the log normal distribution
#' @param sigm2 scale parameter for the log normal distribution
#' @return a numeric vector that represents the incidence rate at age and time.
#'
#' @export
#'


# maximum_inc <- function()
# {
#
#   return( ifelse(matrix_of_times <= 0, 0.006,
#                  ifelse( matrix_of_times <= 18, 0.006 + 0.123 * (matrix_of_times - 12),
#                          ifelse(matrix_of_times <= 24, 0.8,
#                                 ifelse(matrix_of_times <= 31, 0.8 - (0.05 * (matrix_of_times -24)), 0)))))
# }




incidence_mahiane <- function(matrix_of_ages, matrix_of_times, age_debut = 0,
                              beta = 2.3,
                              sigm2 = 0.5)
{


  maximum_inc <- function()
  {
    #shape defining function for the incidence.
    ifelse(matrix_of_times <= 0, 0.006,
           ifelse( matrix_of_times <= 18, 0.006 + 0.123 * (matrix_of_times - 12),
                   ifelse(matrix_of_times <= 24, 0.8,
                          ifelse(matrix_of_times <= 31, 0.8 - (0.05 * (matrix_of_times -24)), 0))))
  }


  # to get reasonable prevalence multiply the realised incidence by 0.1

  norm_fac <- sqrt(2 * pi* sigm2) *(exp((beta - ((sigm2)/2)) * maximum_inc()))

  incidence <- 0.2 * ((norm_fac / ((matrix_of_ages - age_debut) * sqrt(2 * pi * sigm2))) *
                        exp(-(((log(matrix_of_ages - age_debut) - beta)^2) / (2 * sigm2))))

  return(incidence)
}



#' step_incidence
#'
#' a function that takes as arguments age and time and returns a numeric vector of length 1
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param matrix_of_times numeric, indicates time or times at which the incidence rate is desired
#' @param matrix_of_ages  numeric, indicates age or ages at which the incidence rate is desired
#'
#' @export
#'



step_incidence <- function(matrix_of_ages,
                           matrix_of_times){


  incidence = ifelse(matrix_of_ages <= 15, 0, 0.02)

  return(incidence)
}





