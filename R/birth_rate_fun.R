
#' birth_rate_fun
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#'
#' @param delta the time step between consecurtive birth_dates
#' @param birth_dates the calender dates of births.
#' @return Returns a vector of bith counts  from the inputs total_births, delta and birth_dates of length min-birth_dates:max-birth_dates
#'
#'
#' @export

birth_rate_fun <- function(birth_dates, delta){
  times = seq(0, max(birth_dates) - min(birth_dates), delta)
  birth_rates = seq(times)/sum(times)

  return(birth_rates)
}

