

#' Demographic Microsimulation
#'
#' Given a set of demographic rates, plus an initial population,
#' use microsimulation to create a demographic account. 
#' The microsimulation involves random draws, so the
#' demographic account is probabilistic.
#'
#' The interface for the function is not fully developed,
#' and the function is a little inflexible.
#'
#' All dimensions with the same name should have the same length:
#' for instance, all dimensions called "age" should have the same
#' length. This includes the "age" dimension of \code{fertility_rates}.
#' Rates outside the reproductive ages (normally 15 to 49)
#' should be set to zero.
#'
#' The "triangle" dimension holds Lexis triangles, and
#' must have labels "Lower" and "Upper".
#'
#' The "sex" dimension can be length 1, 2, or more.
#'
#' @param initial_popn An array with dimensions "age",
#' "sex", and "iteration"
#' @param fertility_rates An array with dimensions
#' "age", "triangle", "sex", "time", and "iteration"
#' @param mortality_rates An array with dimensions
#' "age", "triangle", "sex", "time", and "iteration".
#' @param step_length Age-time steps. Usually 1 or 5.
#' @param dominant_sex The sex to which births are
#' attributed. Defaults to \code{"Female"}.
#'
#' @return A list of arrays.
#'
#' @export
microsim <- function(initial_popn,
                     fertility_rates,
                     mortality_rates,
                     step_length,
                     dominant_sex = "Female") {
    ## check names of dimensions
    stopifnot(identical(names(dimnames(initial_popn)),
                        c("age", "sex", "iteration")))
    stopifnot(identical(names(dimnames(fertility_rates)),
                        c("age", "triangle", "sex", "time", "iteration")))
    stopifnot(identical(names(dimnames(mortality)),
                        c("age", "triangle", "sex", "time", "iteration")))
    stopifnot(identical(dimnames(mortality_rates)$triangle,
                        c("Lower", "Upper")))
    ## extract lengths of dimensions
    n_age <- dim(initial_popn)[[1L]]
    n_tri <- dim(fertility_rates)[[2L]]
    n_sex <- dim(initial_popn)[[2L]]
    n_time <- dim(fertility_rates)[[5L]]
    n_iter <- dim(initial_popn)[[3]]
    ## check lengths of dimensions
    stopifnot(identical(dim(fertility_rates),
                        c(n_age, n_tri, n_sex, n_time, n_iter)))
    stopifnot(identical(dim(mortality_rates),
                        c(n_age, n_tri, n_sex, n_time, n_iter)))
    ## create dimnames for output
    time_periods <- dimnames(mortality_rates)$time
    time_points <- make_time_points(time_periods)
    
    
    ans <- micro_bd
    
}


       
