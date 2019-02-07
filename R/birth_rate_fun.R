
#' birth_rate_fun
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#'
#' @param delta the time step between consecurtive birth_dates
#' @param birth_dates the calender dates of births.
#' @return Returns a vector of bith counts  from the inputs total_births, delta and birth_dates of length min-birth_dates:max-birth_dates
#' @examples
#' generate_birth_counts(1000, 1984:1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)
#'
#' birth_count <- birth_counts (total_births = 1000,
#'  birth_dates = 1945:1950,
#'  delta = 1)
#'




birth_rate_fun <- function(birth_dates, delta){
  times = seq(0, max(birth_dates) - min(birth_dates), delta)
  birth_rates = seq(times)/sum(times)

  return(birth_rates)
}
