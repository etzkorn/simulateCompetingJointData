#' Simulate joint recurrent event and survival data
#'
#' @description Simulate times of recurrent events and terminal events using joint model.
#' Recurrent events have weibull gap times.
#'
#' @param par0 named parameter vector.
#' Names must include `shapeR`, `scaleR`, `shapeM`, `scaleM`, `shapeD`, `scaleD`,
#' `sigma`, `alphaM`, `alphaD`, `betaR`, `betaM`, `betaD`.
#' Shape and scale parameters correspond
#' to the baseline weibull hazard functions for the recurrent event (Delirium, R) formulated
#' as a gap time process and
#' two competing terminal events (Mortality, M, and Discharge, D).
#' `sigma` corresponds to the variance of the normally-distributed log-frailties.
#' `alphaM` and `alphaD` define the association between the log-frailty and the log-hazard of
#' the terminal event.
#' `betaR`, `betaM`, and `betaD` define the effect of a randomly assigned treatment indicator
#' on the log-hazard of the recurrent and terminating events.
#'
#' @param n number of individuals in data
#'
#' @param truncate time of administrative censoring for all individuals in the data set.
#'
#' @return A tibble where each row corresponds to a risk period for the recurrent event.
#' Events are assumed to have no duration.
#' `tstart` indicates the beginning of the risk period.
#' `tstop` indicates the end of a risk period.
#' `event` indicates whether the risk period ended with the recurrent event.
#' `terminal1` and `terminal2` indicate whether the risk period ended with either
#' terminal event.
#' `trt` is an indicator for whether the individual recieved a treatment or not.
#'
#' @examples
#' simulate.data(n=3)
#' @export

simulate.competing.data <- function(n, truncate = 28, par0){
	K = truncate
	tibble(
	id = 1:n,
	trt = rbinom(n,1,.5),
	w = rnorm(n, 0, par0["sigma"]),
	T1 = rweibRH(n,
		 shape = par0["shapeM"],
		 scale = par0["scaleM"],
		 rh = exp(w*par0["alphaM"] + par0["betaM"] * trt)),
	T2 = rweibRH(n,
		 shape = par0["shapeD"],
		 scale = par0["scaleD"],
		 rh = exp(w*par0["alphaD"] + par0["betaD"] * trt)),
	y = pmin(T1, T2, K),
	terminal1 = as.numeric(T1 < T2 & T1 < K),
	terminal2 = as.numeric(T2 < T1 & T2 < K),
	t =  map2(w,trt,
	          ~ rweibRH(50,
	          	            shape = par0["shapeR"],
	          	            scale = par0["scaleR"],
	          	            rh = exp(.x + par0["betaR"] * .y)) %>%
		cumsum)

	)%>%
	dplyr::select(-T1, -T2, -w) %>%
	unnest(t) %>%
	group_by(id) %>%
	mutate(tstart = c(0, t[-n()])) %>%
	dplyr::filter(tstart<=y)%>%
	mutate(terminal1 = terminal1*(t > y),
		   terminal2 = terminal2*(t > y),
		   event = 1*(t < y),
		   t = ifelse(t > y, y, t)) %>%
	dplyr::select(-y) %>%
		#the timing of the terminal event is no longer needed as a separate variable.
	ungroup
}
