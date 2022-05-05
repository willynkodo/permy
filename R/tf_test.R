#' Compute a t-test and a var-test simultaneously using permutations
#'
#' @param x a sample of length nx following a specific distribution
#' @param y a sample of length ny following a specific distribution
#' @param mu
#' @param sigma
#' @param n_perms the number of permutations
#'
#' @return the pvalue, the test's statistic and the distribution
#' @export
#'
#' @examples tf_test(x,y,mu=0, sigma=1,10000)
tf_test <-function(x,y,mu,sigma,n_perms){

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
  resultat=pf$get_value(c(mu,sigma),keep_null_distribution=TRUE)
  return (resultat)
}
