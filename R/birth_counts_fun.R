

#' birth_counts_fun
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#' @param total_births the total number of births between maximum and minimun birth_dates given.
#' @param delta the time step between consecurtive birth_dates
#' @param birth_dates the calender dates of births.
#' @param birthrate a function  for the distribution of births in interval
#' @return Returns a vector of bith counts  from the inputs total_births, delta and birth_dates of length min-birth_dates:max-birth_dates
#'
#'
#'



birth_counts_fun <- function(total_births,
                             birth_dates,
                             delta,
                             birthrate)
  {

  birth_counts =  birthrate(birth_dates, delta) * total_births

  return(birth_counts)
}
