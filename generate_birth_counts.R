# generate_birth_counts


#'Generate birth counts
#' @param total_births the total number of births between maximum and minimun birthdates given.
#' @param delta the time step between consecurtive birthdates 
#' @param birthdates the calender dates of births.
#' @return Returns a vector of bithcounts  froms the inputs \code{total_births},\code{ delta}, and \code{birthdates} of length \code(max(birthsdates):min(birthsdates))
#' @examples
#' generate_birth_counts(1000, 1984:1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)



#can we make one function as per suggestions but this can save as a toy example 

generate_birth_counts <- function(total_births, 
                              birthdates, delta){
  
  birth_rate <- function(birthdates, delta){
    times  = seq(0, max(birthdates) - min(birthdates), delta)
    birth_rates = seq(times)/sum(times)
    
    return(birth_rates)
  }
  
  birthcounts =  birth_rate(birthdates, delta) * total_births
  
  return(birthcounts)
}
