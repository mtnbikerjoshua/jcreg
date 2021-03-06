% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assumption_checking.R
\name{var_selection}
\alias{var_selection}
\title{Variable Selection}
\usage{
var_selection(
  data,
  method = "all",
  metric = "BIC",
  type.measure = "default",
  row = "method",
  lambda = "lambda.1se"
)
}
\arguments{
\item{data}{A data.frame or object coercible to a data.frame whose \strong{last}
column is the response variable and whose other columns are the predictor
variables.}

\item{method}{A vector of variable selection methods. Options are
"best_subsets", "forward", "backward", "seqrep", "lasso", "elastic".}

\item{metric}{The metric to use for stepwise selection methods. Passed on to
\code{IC} in \code{bestglm::bestglm}. Options are "AIC", "BIC", "BICg", "BICq",
"LOOCV", "CV".}

\item{type.measure}{Loss to use for cross-validation in shrinkage methods.
Passed on to \code{type.measure} in \code{glmnet::cv.glmnet}. Options are "default",
"mse", "deviance", "class", "auc", "mae".}

\item{row}{Which way to orient the table of results. By default puts methods
as rows and variables as columns. Can also be "var" which puts variables as
rows and methods and columns.}

\item{lambda}{For shrinkage methods, use "lambda.1se" or "lambda.min".}
}
\value{
Output is a list of class \code{var_selection} and includes a table of
booleans indicating which variables were included in the best model from
each selection method, a list containing the best model from each selection
method and the output of each selection method. Printing this object will
provide a table of the results. Also note that, for the shrinkage methods,
there is a plot of loss against lambda saved in the method object eg.
\code{my_var_selection$lasso$plot}. For a more detailed summary of the output you
will soon be able to use \code{summary}. That feature is in development.
}
\description{
Uses various stepwise selection and shrinkage methods to perform variable
selection for a linear model.
}
\examples{
# Make sure to put the predictor variable last
bodyfat_reordered <- bodyfat[c("age", "weight", "height", "neck",
                               "chest", "abdom","brozek")]
var_selection(bodyfat_reordered)
var_selection(bodyfat_reordered,
              method = c("best_subsets", "seqrep", "elastic"),
              metric = "AIC")
var_selection(bodyfat_reordered, row = "var")

}
