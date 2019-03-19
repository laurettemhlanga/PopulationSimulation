
#' infected_population 
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param susceptible 
#' @param modified_incidence 
#' @param cumulative_survival 
#' @return returns an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#'




infected_population <- function(susceptible = susceptible_pop_counts,
                                modified_incidence = mod_incidence,
                                cumulative_survival = Cum_prob_survival_i)
  
{
  #fix the bug on calcilating the last column 
  infected_population_array <- array(NA, dim = dim(cumulative_survival))
  
  for (ages in 1: dim(cumulative_survival)[2]){
    for (tau in 1:dim(cumulative_survival)[3]){
      
      if (tau == 1){
        
        infected_population_array[ , , tau] <- susceptible[ , aa ] * modified_incidence[ , aa] * cumulative_survival[, , tau]
    
      }else{
        
        
        infected_population_array[ , , tau] <- susceptible[ , aa - tau ] * modified_incidence[ , aa - tau] * cumulative_survival[, , tau]
        
    }
    
  }
}
  
  return(infected_population_array)
}