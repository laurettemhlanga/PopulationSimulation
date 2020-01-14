#' probability_of_recently_infected
#'
#' a function that calculates the probability of testing recently infected at a time since infection tau.
#'
#' @param time_in_years a vector of times since infection.
#' @param recency_type  a vector of times since infection.
#' @param scale as defined by the weibull scale parameter, determines the scale and determines how spread out the distribution is
#' @param shape as defined by the weibull shape parameter, determines/affects the shape of a distribution
#' @param big_tyears a vector of times since infection.
#' @param q a vector of times since infection.
#' @param gradient default value which is zero and is interpretated as .
#' @param intercept as defined by the weibull scale parameter, determines the scale and determines how spread out the distribution is
#' @param value as defined by the weibull shape parameter, determines/affects the shape of a distribution
#'
#'
#' @return a vector which denotes being the probability of testing recently infected  being recently infected fo



#' @export

probability_of_recently_infected <- function(time_in_years, recency_type = "weibull",
                                             q = 0.0167, shape = 5, gradient = -0.5,
                                             intercept = 0.5, scale = 0.545,
                                             big_tyears = 2, value = 1){
  # time_in_years, scale =  0.4707,
  # q = 0.0167,#0.476,
  # shape = 3.7183


  if(recency_type == "weibull"){

    # recent <- PRT_weibull(time_in_years = time_in_years,
    #                       scale = scale,
    #                       shape = shape)

    recent <-  exp(-(time_in_years / scale) ^ shape)

  }else if(recency_type == "linear"){

    # gradient = -(1/big_tyears)

    recent <-  ifelse(time_in_years > big_tyears, 0, ((gradient/big_tyears) * time_in_years) + intercept)

  }else if (recency_type == "Kassanjee"){

    recent <- ((1 - q) * exp(-(time_in_years / scale) ^ shape)) +  q
  } else{


    recent <- ifelse(time_in_years > 0.5, 0, value)

  }

  return(recent)

}





#' calculate_MDRI
#'
#' @param function_prt dates the surveys are to be conducted
#' @param big_Tyears the maximum age of each cohort

#'
#'
#' @export



calculate_MDRI <- function(function_prt , big_Tyears){

  return(stats::integrate(lower = 0,
                   upper = big_Tyears,
                   f = function_prt)$value)
}


# usage
# calculate_MDRI(f = probability_of_recently_infected,  big_T = 2)

# 161.7364/365

# probability_of_recently_infected(time_in_years = seq(0, 4, 1/12),
#                                  recency_type = "")L

# integrate(upper = 730, lower = 0, f = probability_of_recently_infected(x, recency_type = "weibull"))


# 1/(1-gamma8)*0.5
#
