
#'compact_population
#'
#'
#' a function that returns an array
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



compact_population <- function(susceptible, infected){

  # it turns the data structure into a compact form since istead of the individual pieces
  # that are initially observed

  full_pupolation <- array(NA, dim  = c(dim(infected)[1],
                                        dim(infected)[2],
                                        dim(infected)[3]+1))
  full_pupolation[ , , 1] <- susceptible

  full_pupolation[ , , -1] <- infected

  #full_pupolation[ , ,1] <- rowSums(full_pupolation, dims = 2, na.rm = T)

  return(full_pupolation)

}
