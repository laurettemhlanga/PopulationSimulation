







cumulative_infected_survival_probability <- function(infected_survival_prob = infected_survival_probs)
  {


  cumulative_infected_survival_prob <- array(NA, dim = c(dim(infected_survival_prob)[1],
                                                         dim(infected_survival_prob)[2],
                                                         (dim(infected_survival_prob)[3] +1)))

  cumulative_infected_survival_prob[, , 1] <- matrix(rep(1, (dim(infected_survival_prob)[1] * dim(infected_survival_prob)[2])),
                                              nrow = dim(infected_survival_prob)[1],
                                              ncol = dim(infected_survival_prob)[2])

  for (tt in (2:dim(infected_survival_prob)[1])){

    for (ta in (2:dim(infected_survival_prob)[2])){

      for (ts in (3:dim(infected_survival_prob)[3])){

        if (!is.na(infected_survival_prob[tt, ta, ts]) == T){



        cumulative_infected_survival_prob[tt, ta, ts] <-  cumulative_infected_survival_prob[tt - 1, ta - 1, ts - 1] * infected_survival_prob[tt, ta, ts]

        }else{

          cumulative_infected_survival_prob[tt, ta, ts] <-  NA

        }



      }

    }

  }

  return(cumulative_infected_survival_prob)
}
