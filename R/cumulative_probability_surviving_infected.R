


#' cumulative_probability_surviving_infected
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param probability_surviving_infected_array  3 dimensional probability matrix 
#' @return returns an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#


cumulative_probability_surviving_infected <- function(probability_surviving_infected_array = y)
{
  # calculates the cumulative probability of surviving in the infected state after infection at time 
  # t - tau, age - tau  for a duration of tau years. tt is constant as the matrix is being populated horizontally
  # (when the third loop is effected). 
  
  cumulative_probability_surviving_array <-  array(NA, dim = dim(probability_surviving_infected_array))
  
  cumulative_probability_surviving_array[, , 1] = probability_surviving_infected_array[, , 1]
  
  times <- dim(probability_surviving_infected_array)[1]
  
  for(tt in 1:dim(probability_surviving_infected_array)[1] ){
    for (aa in 2:dim(probability_surviving_infected_array)[2] ){
      for (ta in 2:dim(probability_surviving_infected_array)[3]){
      
        
        cumulative_probability_surviving_array[ tt, aa , ta] <-   cumulative_probability_surviving_array[tt, aa - 1 , ta - 1] * probability_surviving_infected_array[ tt, aa , ta]
       
       
      }
    }
    
  }
  
  return(cumulative_probability_surviving_array)
  
}




Cum_prob_survival_i <- cumulative_probability_surviving_infected(y)





