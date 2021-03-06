% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assumption_checking.R
\name{jcreg_hist}
\alias{jcreg_hist}
\title{Histogram of Residuals}
\usage{
jcreg_hist(model)
}
\arguments{
\item{model}{A linear regression model of class \code{stats::lm}}
}
\value{
Returns a ggplot object.
}
\description{
Uses ggplot2::ggplot to create a nicely formatted histogram of the residuals of a
model overlaid with a normal curve.
}
\examples{
jcreg_hist(lm(brozek ~ ., data = bodyfat))

}
