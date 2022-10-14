#' Summarise joint recurrent event and survival data
#'
#' @description Summarise data set produced by `simulate.competing.joint`.
#'
#' @param data a data set produced by `simulate.competing.joint`.
#'
#' @return A tibble
#'
#' @examples
#' summarise.competing.data(simulate.competing.data(n=100))
#' @export

summarise.competing.data <- function(data){
	data %>%
	group_by(id) %>%
	summarise(
	         event = sum(event),
	         trt = mean(trt),
	         terminal1 = sum(terminal1),
	         terminal2 = sum(terminal2),
	         t = max(t)) %>%
	ungroup %>%
	group_by(terminal1, terminal2, trt) %>%
	summarise(
		frequency = n(),
		totalStudyTime = sum(t),
		avgStudyTime = mean(t),
		anyEventN = sum(event>0),
		anyEventPct = mean(event>0),
		eventN = sum(event),
		eventsPer100Day = 100*eventN / totalStudyTime,
		.groups = "drop") %>%
	ungroup %>%
	group_by(trt) %>%
	mutate(frequencyPct = 100*frequency/sum(frequency),
		   treatmentN = sum(frequency)) %>%
	ungroup %>%
	mutate(treatmentPct = 100*treatmentN/sum(frequency)) %>%
	arrange(trt, terminal1, terminal2)
}
