#
#
# data_structure = y
# date_of_birth = 1:5
# ages = 3:7
# times_since_infection = 3
# first_birth_time = 1985,
# last_birth_time = 1990,
# time_step = 0.5,
# max_age = 4
#
# list_of_date_births <- seq(first_birth_time, last_birth_time, time_step)
# list_of_ages <- seq(0, max_age, time_step)
# list_of_time_since_infection <- seq(time_step, max_age, time_step)
#
#
# if ((is.null(ages) && is.null(times_since_infection)) == T) {
#
#   vec <-  which(list_of_date_births == date_of_birth)
#   output <-  data_structure[vec, , ]
#
# } else if ( (is.null(date_of_birth) && is.null(times_since_infection)) == T) {
#
#   vec <-  which(list_of_ages == ages)
#   output <-  data_structure[, vec, ]
#
# } else if ((is.null(date_of_birth) && is.null(ages)) == T) {
#
#   vec <-  which(list_of_time_since_infection == times_since_infection )
#   output <-  data_structure[, , vec]
#
# } else if ( is.null(times_since_infection) == T) {
#
#   vec1 <-  which(list_of_date_births == date_of_birth)
#   vec2 <- vec <-  which(list_of_ages == ages)
#
#   output <-  data_structure[ vec1, vec2, ]
#
# } else if ( test_expression3) {
#   statement5
# }else if ( test_expression2) {
#   statement6
# } else if ( test_expression3) {
#   statement7
# }else {
#   statement4
# }
