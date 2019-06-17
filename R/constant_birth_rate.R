
#' constant_birth_rate
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#'
#' @param dates_as_float  is a double/vector of times the birth rate is to be evaluated at.
#' @param constant_value the number of births at a given birth date, given that it is constant through out the specified birth dates
#' @return Returns a vector of birth counts  of the length of the birth dates
#'
#'
#' @export
#'
#'

# #' @param delta the time step between consecurtive birth_dates

constant_birth_rate <- function(dates_as_float, constant_value = 1000)
{
  # births per year, evaluated at the list of dates in the vector 'dates_as_float'

  return(rep(constant_value,length(dates_as_float)))

}

#birth_rate_fun(c(1,2))
