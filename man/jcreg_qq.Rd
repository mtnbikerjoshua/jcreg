% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assumption_checking.R
\name{jcreg_qq}
\alias{jcreg_qq}
\title{Quantile - Quantile Plot}
\usage{
jcreg_qq(model)
}
\arguments{
\item{model}{A linear regression model of class \code{stats::lm}.}
}
\value{
Returns a ggplot object.
}
\description{
Uses \code{autoplot} from \code{ggfortify} to plot a square, nicely-formatted Q-Q Plot
of the residuals of a linear model.
}
\examples{
jcreg_qq(lm(brozek ~ ., data = bodyfat))

}
