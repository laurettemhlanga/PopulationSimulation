#generate_infected_population



#'calculates the infected populations
#' @param  infected_pop_counts the 
#' matrix of the total  number of people infected at a given age and time   
#' @param  infected_mortality_matrix a survival probability matrix for the infected a 3 dimensional structure of age, time and tau(time since infection)
#' @return returns a number or or a vector of mortality rates for a a given age and time given  \code{susceptible_survival_rate_matri}
#' @examples
#'  ...


generate_infected_population  <- function(infected_mortality_matrix,
                                          infected_pop_counts
                                          
                                          ){
  
  #the function simulates the infected population at time tt aged aa and were infected at age aa_0
  # i.e 0 < aa_0 < aa 
  # Need to embed the Infected[aa,tt, 0] so that the output of the array includes all the possible times
  #since infection. Simplifies the calculation of the prevalence when the array is summed over times 
  
  infected  = array(0, dim = c(nrow(infected_pop_counts), ncol(infected_pop_counts), ncol(infected_pop_counts)+1))
  
  infected[ , , 1] = infected_pop_counts
  
  times <- 2:nrow(infected_pop_counts)
  ages  <-  2:ncol(infected_pop_counts)
  
  for (aa in ages){
    Tau = 2:(aa+1)
    for ( ta  in  Tau){
      
      infected[times, aa , ta] = infected_mortality_matrix[times - 1, aa - 1, ta -1] * infected[times - 1 , aa - 1, ta - 1]
      
    }
    
  }
  return(infected)
}

