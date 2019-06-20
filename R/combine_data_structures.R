#'
#'
#'
#'
#'
#' #'
#' #' combine_data_structures
#' #'
#' #' a function that returns an array and the data structure into a compact structure
#' #'
#' #' @param susceptible proportion of the population that is susceptible.
#' #' @param infected proportion of the population that is infected.
#' #'
#' #' @return a array
#' #'
#' #' Values stored in the matrix are numeric double, from 0-1, which represent the natural mortality rate at given age and time
#' #'
#' #'
#' #' @export
#' #'
#'
#'
#' library(abind)
#'
#' combine_data_structures <- function(susceptible,
#'                                     infected){
#'
#'  return(population  = abind(susceptible, infected, along  = 3))
#'
#' }
#'
#'
#'
