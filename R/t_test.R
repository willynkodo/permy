#' Compute a t-test using permutations
#'
#' @param x a sample of length nx following a specific distribution.
#' @param y a sample of length ny following a specific distribution.
#' @param alter a string character, the type of test computed either "right_tail", "left_tail" or two_tail".
#' @param mu a number indicating the difference in means for a two-sample t.test.
#' @param var_equal a logical scalar, are we considering the variances to be equal or not ?
#' @param conf_level the confidence level of the interval.
#' @param n_perms the number of permutations.
#'
#' @return the test's statistic, its p-value, the degrees of freedom, the estimated difference in means, the alternative hypothesis, the method performed, the names of the data
#' @export
#'
#' @examples t_test(x,y,alter="two_tail",mu=0,var_equal=TRUE,conf_level=0.95,n_perms=10000)
t_test<-function(x,y,alter,mu,var_equal,conf_level,n_perms){

  null_spec <- function(y, parameters){
    purrr::map(y, ~ .x - parameters[1])
  }
  stat_functions <- if (var_equal==TRUE) {list(flipr::stat_t)} else {list(flipr::stat_welch)}
  stat_assignments <- list(delta=1)
  pf<-flipr::PlausibilityFunction$new(
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x, y,
    seed=1234
  )
  pf$set_nperms(n_perms)
  pf$set_alternative(alter)
  pf$set_max_conf_level(conf_level)
  a=pf$get_value(mu,keep_null_distribution=TRUE)$observed
  b=pf$get_value(mu,keep_null_distribution=TRUE)$pvalue
  c=(length(x)+length(y))-2
  d=mean(y)-mean(x)
  return (list(stat=a,pvalue=b,df=c,estimate=d,alternative=alter,method="Student Test to compare two means using permutations",data_name="x and y"))
}


