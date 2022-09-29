#' Draw from weibull distrubution with an optional relative hazard.
#'
#' @description Simulate numbers from weibull distribution with a
#' baseline hazard described by the shape and scale parameters and
#' an optional relative hazard.
#'
#' @param n replicate count.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param shape shape parameter from rweibull.
#' @param scale shape parameter from rweibull.
#' @param rh relative hazard which compared to the hazard defined by shape and scale alone.
#'
#' @return `rweibRH` returns vector of n replicates.
#'
#' @examples
#' # Try the base R function rweibull
#' set.seed(100)
#' rweibull(5, shape = 3, scale = 2)
#'
#' # Show equivalence when rh = 1
#' set.seed(100)
#' rweibRH(5, shape = 3, scale = 2, rh = 1)
#'
#' # Increasing the relative hazard decreases time to events.
#' set.seed(100)
#' rweibRH(5, shape = 3, scale = 2, rh = 2)
#'
#' @export

rweibRH <- function(n, shape ,scale , rh){
	rweibull(n, shape = shape, scale = scale * rh^(-1/shape))
}

pweibRH <- function(q, shape, scale, rh){
	pweibull(q, shape = shape, scale = scale * rh^(-1/shape))
}

dweibRH <- function(x, shape, scale, rh){
	dweibull(x, shape = shape, scale = scale * rh^(-1/shape))
}

qweibRH <- function(p, shape, scale, rh){
	qweibull(p, shape = shape, scale = scale * rh^(-1/shape))
}
