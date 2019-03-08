
#' birth_rate_fun
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#'

#' @param tiam  is a double/vector of times the birth rate is to be evaluated at.
#' @return Returns a vector of bith counts  from the inputs total_births, delta and birth_dates of length min-birth_dates:max-birth_dates
#'
#'
#' @export
#'
#'

# #' @param delta the time step between consecurtive birth_dates

constant_birth_rate_fun <- function(dates_as_float, constant_value = 1000)
{
  # births per year, evaluated at the list of dates in the vector 'dates_as_float'

  return(rep(constant_value,length(dates_as_float)))

}

#birth_rate_fun(c(1,2))
