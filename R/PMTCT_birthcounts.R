

#' PMTCT_birthcounts
#'
#' a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#'
#' @param dates_needing_birth_counts a vector of births dates to be assigned the birth count
#' @param birth_count total number of births at given date births
#' @param time_step the time step between consecurtive birth dates in the vector list_of_birth_times
#' @param pmtct_birth_rate a function  for the distribution of births in the interval
#' @return Returns a vector of bith counts
#'
#'
#' @export



pmtct_birthcounts <- function(dates_needing_birth_counts,
                              pmtct_birth_rate,
                              birth_count,
                              time_step)
{

  # calculates the initial number of infected babies counts = S(t, 0) at the specified times
  # the assumption  is all the births occurs at the stipulated time i.e B(t) * time step versus mid point
  # calculation

  return((pmtct_birth_rate(dates_needing_birth_counts) * birth_count)* time_step)


}

# pmtct_birthcounts(dates_needing_birth_counts = 1:5,
#                   pmtct_birth_rate = constant_pmtct_rate,
#                   birth_count = rep(100, length(1:5)),
#                   time_step = 1)
