#generate_excess_mortality





generate_excess_mortality_ta <- function(t, ta, conc = 0.05, agemin = 0,  
                                                                  agemax = 50,   
                                                                  exmin =0.01,  
                                                                  exfin =0.05){
  #varying mortality (toy function) 
  
  Ex_mort = ifelse(t <= agemin, 0, 
                   ifelse(t <= agemax, exmin + ((exfin - exmin)/(agemax - agemin)) * ta *(t - agemin),
                          0))
  
  return(Ex_mort)
}

generate_excess_mortality(t = 14, ta = 50)
