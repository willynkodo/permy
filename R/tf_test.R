#' Compute a t.test and a var.test simultaneously using permutations
#'
#' @param x a sample of length nx following a specific distribution.
#' @param y a sample of length ny following a specific distribution.
#' @param alter the type of test computed either "right_tail", "left_tail" or two_tail".
#' @param mu a number indicating the difference in means for a two-sample t.test.
#' @param sigma a number indicating the ratio between the variances for a two-sample var.test.
#' @param conf_level the confidence level of the interval.
#' @param n_perms the number of permutations.
#'
#' @return the test's statistic, its p-value,, the estimated difference in means, the estimates ration in variances, the alternative hypothesis, the method performed, the names of the data
#' @export
#'
#' @examples tf_test(x,y,alter="two_tail",mu=0,sigma=1,conf_level=0.95,n_perms=10000)
tf_test <-function(x,y,alter,mu,sigma,conf_level,n_perms){
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ (.x - parameters[1])/parameters[2])
  }
  stat_functions <- list(flipr::stat_t, flipr::stat_f)
  stat_assignments <- list(delta = 1, rho = 2)
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
  c=var(x)/var(y)
  d=mean(y)-c*mean(x)
  return (list(stat=a,pvalue=b,df_nom=c(c,d),estimate_mean=d,estimate_variance=c,alternative=alter,IC=NULL,method="T Test and  F Test to compare simulataneously two means and two variances using permutations",data_name="x and y"))
}
