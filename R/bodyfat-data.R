#' Bodyfat Data
#'
#' Measuring body fat is not simple. One method requires submerging the body
#' underwater in a tank and measuring the increase in water level. A simpler
#' method for estimating body fat would be preferred. In order to develop such a
#' method, researchers recorded age (years), weight (pounds), height (inches),
#' and three body circumference measurements (around the neck, chest, and
#' abdominal (all in centimeters)) for 252 men. Each man’s percentage of body
#' fat was accurately estimated by an underwater weighing technique (the
#' variable brozek is the percentage of body fat). The hope of this data is to
#' create a model that will accurately predict body fat percentage, by using
#' just the basic variables recorded, without having to use the tank submerging
#' method.
#'
#' @docType data
#'
#' @usage data(bodyfat)
#'
#' @format A dataframe with 7 variables and 250 observations
#'
#' @source Taken from a linear regression (STAT330) class at Brigham Young
#'   University taught by Dr. Brinley Zabriskie.
#'
#' @examples
#' summary(bodyfat)
#' lm(brozek ~ ., data = bodyfat)
#' point_matrix(bodyfat)
"bodyfat"
