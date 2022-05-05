#' computation of a var-test using permutations
#'
#' @param x a sample of length nx following a specific distribution
#' @param y a sample of length ny following a specific distribution
#' @param sigma
#' @param n_perms the number of permutations
#'
#' @return the pvalue, the test's statistic and the distribution
#' @export
#'
#' @examples var_test(x,y,sigma=1,10000)
var_test <-function(x,y,sigma,n_perms){

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
  resultat=pf$get_value(sigma,keep_null_distribution=TRUE)
  return (resultat)
}
