
# excess mortality array

excess_mortality_array <-function(max_age,
                                   list_of_times,
                                   excess_mortality,
                                   time_step)
{
  # produces an array of dimension length(list of times), length(0 to max_age), and time since infection
  # length(0 to max_age) base on the excess mortality function

  ages <- 0:max_age

  times <- list_of_times

  time_si <- 0:max_age

  excess_mortality_container<- array(NA, dim = c(length(time), ))

  counter <- 1

  for (time in 1:length(times)){

    for (age in  1:length(ages)){

      for (time_s in 1:length(time_si)){

        excess_mortality_container[times[counter], ] <-  exp( -excess_mortality(time[counter] + (0.5 *time_step),
                                                                    age[counter] + (0.5 *time_step),
                                                                    time_s[counter] + (0.5 *time_step)) * time_step)

      counter <- counter + 1


      }

    }
  }

  return(excess_mortality_container)

}
