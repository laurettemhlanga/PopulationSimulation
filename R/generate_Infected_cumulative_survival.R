#' generate_Infected_cumulative_survival_array
#'
#' a function that returns an array of cumulative probabilities of survival -i.e. not dying by natural cause/excess mortality
#'
#' @param generate_Infected_cumulative_survival_array a survival probability matrix
#' @return a array calculated from the generate_Infected_survival_array
#' Values stored in an array are numeric-double, from 0-1, each value represents the  probability of surviving to **a/beyond???** specified age and time having been infected for "time since infection".
#' @examples x <- generate_infected_cumulative_survival_matrix
#' (matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8))


generate_infected_cumulative_survival_matrix <- function(infected_survival_rate_array)

{

  infected_cumulative_survival_matrix <- array(NA, dim = dim(infected_survival_rate_array))

  column_1 <- (nrow(infected_survival_rate_array) - ncol(infected_survival_rate_array))+1

  infected_cumulative_survival_matrix[1:column_1, 1,] = rep(1,  column_1)

  for  (tt in 2:dim(infected_survival_rate_array)[1]){
    for (aa in 2:dim(infected_survival_rate_array)[2]){
      for (ta in 2:dim(infected_survival_rate_array)[3]){

      #if (!is.na(infected_survival_rate_array[tt, aa, ta]) == T){

        infected_cumulative_survival_matrix[tt, aa, ta] = infected_cumulative_survival_matrix[tt-1, aa - 1, ta - 1] * infected_survival_rate_array[tt-1, aa-1, ta-1]

      #}else{
        #infected_cumulative_survival_matrix[tt, aa, ta]  = NA
      #}
        }
    }
  }
  return(infected_cumulative_survival_matrix)
}


y <- generate_infected_cumulative_survival_matrix(infected_survival_rate_array)
