#generate_infected_mortality

#'calculates the probability of survival in the infected populations
#' @param  infected_pop_counts the 
#' matrix of the total  number of people infected at a given age and time   
#' @param  infected_mortality_matrix a survival probability matrix for the infected a 3 dimensional structure of age, time and tau(time since infection)
#' @return returns an array  \code{susceptible_survival_rate_matri}
#' @examples
#'  ...


generate_infected_mortality <- function(Ages,
                                        Time, 
                                        ex_mortality_fun,
                                        base_mortality_fun
                                        ){
  
  
  #NB not running 
  
  #function calculates the dicretised survival probabilities i.e. prob of not getting 
  #infected and the probability of not dying for a specific age and time.  
  infected_mortality_array = array(NA, dim = c(length(Time), length(Ages), length(Ages)))
  
  for (aa in Ages){
    for(tt in Time){ 
      for (ta in Ages){
      
        infected_mortality_array[times + aa, aa, ta] = mortality_func(tt, aa) + ex_mortality_fun(tt, aa, ta)
        
        infection_matrix[times + aa, aa] =  incidence_func(times + aa, aa)
     
       }
    }
  }
  
  return(infected_mortality_array)
  
}
