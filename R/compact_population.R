#'compact_birthcohort
#'
#'
#' a function that returns a matrix
#'
#' @param susceptible proportion of the population that is susceptible.
#' @param infected proportion of the population that is infected.
#'
#' @return a array
#'
#' Values stored in the matrix are numeric double, from 0-1, which represent the natural mortality rate at given age and time
#'
#'
#' @export
#'


compact_birthcohort <- function(susceptible, infected){

  # it turns the data structure into a compact form since istead of the individual pieces
  # that are initially observed

  full_pupolation <- matrix(NA, nrow = nrow( infected)+1,  ncol = length(susceptible))
  full_pupolation[ 1, ] <- susceptible

  full_pupolation[2:nrow(full_pupolation), ] <- infected

  #full_pupolation[ , ,1] <- rowSums(full_pupolation, dims = 2, na.rm = T)

  return(full_pupolation)

}






