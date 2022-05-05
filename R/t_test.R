#' Compute a t-test using permutations
#'
#' @param x a sample of length nx following a specific distribution
#' @param y a sample of length ny following a specific distribution
#' @param mu
#' @param var_equal a logical scalar, are we considering the variances to be equal or not ?
#' @param n_perms the number of permutations
#'
#' @return the pvalue, the test's statistic and the distribution
#' @export
#'
#' @examples t_test(x,y,mu=0,var_equal=TRUE,10000)
t_test<-function(x,y,mu,var_equal,n_perms){

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
  )
  pf$set_nperms(n_perms)
  resultat=pf$get_value(mu,keep_null_distribution=TRUE)
  return (resultat)
}
