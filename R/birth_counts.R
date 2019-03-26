

#' birth_counts_fun
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#' @param dates_needing_birth_counts a vector of births dates to be assigned the birth count
#' @param time_step the time step between consecurtive birth_dates
#' @param birth_rate a function  for the distribution of births in interval
#' @return Returns a vector of bith counts  from the inputs total_births, delta and birth_dates of length min-birth_dates:max-birth_dates
#'
#'
#' @export



birth_counts <- function(dates_needing_birth_counts,
                             birth_rate, time_step)
{

# calculates the initial number of birth counts = S(t, 0) at the specified times
# the assumption  is all the births occurs at the stipulated time i.e B(t) * time step versus mid point
# calculation

  return(birth_rate(dates_needing_birth_counts)* time_step)


}






# Options
# birth_counts_fun <- function(t_1, t_2,
#                              birthrate,
#                              delta)
#   {
#   taim <- seq(t_1, t_2, delta)
#
#   birth_counts <- as.vector(rep(NA, length(taim)))
#
#   index_taim <- (1:length(taim))
#
#   counter <- 1
#
#   for (i in  index_taim){
#
#   birth_counts[i] =  birthrate(taim + (0.5 * delta))
#
#   counter <- counter + 1
#
#   }
#
#   return(birth_counts)
# }
#

#option B

# birth_counts <- function(dates_needing_birth_counts,
#                          birth_rate, time_step)
# {
#
#    return(sapply(dates_needing_birth_counts + (0.5 * delta), FUN = birthrate)*delta)
#
# }

