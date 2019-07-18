#' probability_of_recently_infected
#'
#' a function that calculates the probability of testing recently infected at a time since infection tau.
#'
#' @param time_in_years a vector of times since infection.
#' @param constant default value which is zero and is interpretated as .
#' @param scale as defined by the weibull scale parameter, determines the scale and determines how spread out the distribution is
#' @param  shape as defined by the weibull shape parameter, determines/affects the shape of a distribution
#'
#'
#'
#' @return a vector which denotes being the probability of testing recently infected  being recently infected fo
#'
#'
#' @export


probability_of_recently_infected <- function(time_in_years,
                                             constant = 0,
                                             scale = 0.476,
                                             shape = 2)
  {

  #should we

  recent <- ifelse(time_in_years, exp(-(time_in_years / scale) ^ shape), constant )


  return(recent)
}

