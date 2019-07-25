#' probability_of_recently_infected
#'
#' a function that calculates the probability of testing recently infected at a time since infection tau.
#'
#' @param time_in_years a vector of times since infection.
#' @param type  a vector of times since infection.
#' @param scale as defined by the weibull scale parameter, determines the scale and determines how spread out the distribution is
#' @param shape as defined by the weibull shape parameter, determines/affects the shape of a distribution
#' @param cutoff a vector of times since infection.
#' @param gradient default value which is zero and is interpretated as .
#' @param intercept as defined by the weibull scale parameter, determines the scale and determines how spread out the distribution is
#' @param value as defined by the weibull shape parameter, determines/affects the shape of a distribution
#'
#'
#' @return a vector which denotes being the probability of testing recently infected  being recently infected fo
#'
#'
#'


#' @export

probability_of_recently_infected <- function(time_in_years,
                                             type,
                                             shape = 2,
                                             scale = 0.476,
                                             intercept = 1,
                                             gradient = -0.5,
                                             cutoff = 2,
                                             value = 1){

  if(type == "weibull"){

    recent <- PRT_weibull(time_in_years = time_in_years,
                          scale = scale,
                          shape = shape)
  }else if(type == "linear"){


    recent <- PRT_linear(time_in_years = time_in_years ,
                         gradient = gradient,
                         intercept = intercept,
                         cutoff = cutoff)

  }else {


    recent <- PRT_step(time_in_years= time_in_years,
                       value = value,
                       cutoff = cutoff)

  }

  return(recent)

}



PRT_weibull <- function(time_in_years,
                        scale = 0.476,
                        shape = 2)
  {

  #should we

  recent <-  exp(-(time_in_years / scale) ^ shape)


  return(recent)
}



PRT_linear <- function(time_in_years,
                        gradient ,
                        intercept,
                        cutoff )
{


  recent <- ifelse(time_in_years > cutoff, 0, (gradient * time_in_years) + intercept)


  return(recent)
}




PRT_step <- function(time_in_years,
                     value,
                     cutoff ){



  recent <-  ifelse(time_in_years > cutoff, 0, value)


  return(recent)
}



# probability_of_recently_infected(time_in_years = seq(0, 4, 1/12),
#                                  type = "step")



