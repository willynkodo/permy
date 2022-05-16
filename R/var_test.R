#' Compute a var.test using permutations
#'
#' @param x a sample of length nx following a specific distribution
#' @param y a sample of length ny following a specific distribution
#' @param alter the type of test computed either "right_tail", "left_tail" or two_tail".
#' @param sigma a number indicating the ratio between the variances for a two-sample var.test.
#' @param conf_level the confidence level of the interval.
#' @param n_perms the number of permutations.
#'
#' @return the test's statistic, its p-value, the degrees of freedom of the samples, the estimated ratio in variances, the alternative hypothesis, the method performed, the names of the data
#' @export
#'
#' @examples var_test(x,y,alter="two_tail",sigma=1,sigma=1,conf_level=0.95,n_perms=10000)
var_test <-function(x,y,alter,sigma,conf_level,n_perms){
  null_spec <- function(y, parameters){
    purrr::map(y, ~ .x/parameters[1])
  }
  stat_functions <-list(flipr::stat_f)
  stat_assignments <- list(rho=1)
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
  a=pf$get_value(sigma,keep_null_distribution=TRUE)$observed
  b=pf$get_value(sigma,keep_null_distribution=TRUE)$pvalue
  c=length(x)-1
  d=length(y)-1
  e=var(x)/var(y)
  return (list(stat=a,pvalue=b,df=c(c,d),estimate=e,alternative=alter,IC=NULL,method="F Test Test to compare two variances using permutations",data_name="x and y"))
}
