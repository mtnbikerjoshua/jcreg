library(tidyverse)  # For easier table manipulation
library(ggfortify)  # Plot lm objects using ggplot instead of base R
library(car)  # For added-variable plots, dfbetas and dffits
library(corrplot)  # For colored correlation matrix
library(gridExtra)  # For putting multiple ggplots in one plot

#' Scatterplot matrix
#'
#' `point_matrix` returns a basic scatterplot matrix of the data input.
#'
#' This function uses pairs(data) to generate a scatterplot matrix with square
#' plots and solid points. Note that for best results you should set your plot
#' size to be square.
#'
#' @param data The coordinates of points given as numeric columns of a matrix
#' or data frame. See parameter `x` in `pairs()` for more detail.
#'
#' @examples
#' point_matrix(mtcars)
#'
#' @export
point_matrix <- function(data) {
  par(pty = "s", las = 1)
  pairs(data, pch = 19, lower.panel = NULL)
}

#' Correlation Plots
#'
#' `cor_graphic` uses the "corrplot" package to create a nice graphic with a
#' color-coded correlation matrix and correlation plot side-by-side
#'
#' @param data A numeric vector, matrix or data frame to be used as input to
#' `cor`
#'
#' @export
cor_graphic <- function(data) {
  par(mfrow = c(1, 2))
  corrplot(cor(data), method = "number", type = "upper", diag = F, tl.col = "#1f3366", cl.pos = "n")
  title("Correlation Coefficients")
  corrplot(cor(data), type = "upper", diag = F, tl.col = "#1f3366", cl.pos = "n")
  title("Correlation Matrix")
}

#' Residuals Vs Fitted Values Plot
#'
#' `resid_vs_fitted` uses `autoplot` to create a square residuals vs fitted
#'  values plot with a nice theme
#'
#'  @param model A linear regression model of class `stats::lm`
#'
#'  @export
resid_vs_fitted <- function(model) {
  autoplot(model, which = 1, ncol = 1) +
    theme_minimal() +
    theme(aspect.ratio = 1)
}

#' Residuals vs Predictor Plot
#'
#' A helper function for resid_vs_pred. Plots the residuals vs the predictor for
#' a single predictor variable. Returns a ggplot object.
#'
#' @param data A dataframe containing one or more columns including the
#' predictor variable.
#' @param residuals A vector or a dataframe containing the residuals assosiated
#' with the data. Its length must be the same as the numer of rows in `data`
#' @param predictor A string. The name of the column in `data` to be used as the
#' predictor variable.
rpred_col <- function(data, residuals, predictor) {
  ggplot(data = data,
         mapping = aes(x = pull(data, predictor),
                       y = residuals)) +
    geom_point() +
    geom_smooth(se = FALSE, span = 0.95, n = 7, size = 0.5) +
    geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    theme(aspect.ratio = 1) +
    xlab(predictor) +
    ylab("Residuals")
}

#' Matrix of Residuals Vs Predictor Plots
#'
#' `resid_vs_pred` uses ggplot to create a matrix of residual vs predictor plots
#'
#' @param A linear regression model of class `stats::lm`
#'
#' @export
resid_vs_pred <- function(model) {
  data <- model.frame(model)
  predictors <- attr(model$terms, "term.labels")
  plots <- lapply(predictors, rpred_col, data = data, residuals = resid(model))
  plots["ncol"] <- ceiling(sqrt(length(plots)))
  plots["top"] <- "Residuals vs Predictors"
  do.call(grid.arrange, plots)
}

#' Added-Variable Plots
#'
#' Wrapper for `car::avPlots` with square plots and well layed out.
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @export
jcreg_av <- function(model) {
  predictors <- attr(model$terms, "term.labels")
  rows <- floor(sqrt(length(predictors)))
  cols <- length(predictors) / rows
  par(pty = "s", cex.lab = 2, cex.axis = 1.5)
  avPlots(model, layout = c(rows, cols), pch = 19)
}

#' Boxplot of residuals
#'
#' Uses ggplot to create a nicely formatted box plot of the residuals of a model
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @examples
#' jcreg_boxplot(lm(mpg ~ cyl, data = mtcars))
#'
#' @export
jcreg_boxplot <- function(model) {
  residuals <- data.frame(residuals = resid(model))
  ggplot(data = residuals, mapping = aes(y = residuals)) +
    geom_boxplot() +
    stat_summary(mapping = aes(x = 0),
                 fun = mean, geom = "point",
                 shape = 4, size = 2, color = "darkred") +
    theme_classic() +
    theme(aspect.ratio = 2,
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    #  scale_y_continuous(limits = c(-20000, 30000), breaks = seq(-20000, 30000, 10000)) +
    ylab("Residuals") +
    xlab("")
}

#' Histogram of Residuals
#'
#' Uses ggplot to create a nicely formatted histogram of the residuals of a
#' model overlaid with a normal curve.
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @export
jcreg_hist <- function(model) {
  residuals <- data.frame(residuals = resid(model))
  ggplot(data = residuals, mapping = aes(x = residuals)) +
    geom_histogram(binwidth = sd(residuals$residuals / 4), mapping = aes(y = ..density..)) +
    stat_function(fun = dnorm,
                  color = "blue",
                  args = list(mean = 0,
                              sd = sd(residuals$residuals)),
                  size = 1.2) +
    xlab("Residuals") +
    ylab("Density") +
    theme_light()
}

#' Quantile - Quantile Plot
#'
#' Uses `autoplot` to plot a square, nicely-formatted Q-Q Plot of the residuals
#' of a linear model.
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @export
jcreg_qq <- function(model) {
  autoplot(model, which = 2, ncol = 1) +
    theme_bw() +
    theme(aspect.ratio = 1)
}

#' Cook's Distance Plot
#'
#' Plots the absolute value of cook's distance against observation number.
#'
#' @param model A linear regression model of class `stats::lm`
#' @param nLabels The number of potential influential points to label. Defaults
#' to 3. A value of 0 means do not label potential influential points.
#'
#' @export
jcreg_cooksd <- function(model, nLabels = 3) {
  cooks_d <- cooks.distance(model)
  top_cd <- as.numeric(names(sort(cooks_d, decreasing = TRUE)[1:nLabels]))

  ggplot() +
    geom_point(data = tibble(cooks_d),
               mapping = aes(x = as.numeric(names(cooks_d)),
                             y = cooks_d)) +
    geom_text(mapping = aes(x = top_cd,
                            y = cooks_d[top_cd] + max(cooks_d) / 40,
                            label = top_cd)) +
    theme_bw() +
    ylab("Cook's Distance") +
    xlab("Observation Number") +
    geom_hline(mapping = aes(yintercept = 4 / length(cooks_d)),
               color = "red", linetype = "dashed") +
    theme(aspect.ratio = 1)
}

#' DFBETAS Plot
#'
#' A helper function for `jcreg_dfbetas`. Plots the DFBETAS for a single
#' predictor variable. Returns a ggplot object.
#'
#' @param df_betas A dataframe containing one or more columns including the
#' DFBETAS of the predict variable.
#' @param residuals A vector or a dataframe containing the residuals assosiated
#' with the data. Its length must be the same as the numer of rows in `data`
#' @param predictor A string. The name of the column in `data` to be used as the
#' predictor variable.
dfb_col <- function(df_betas, predictor, nLabels = 3) {
  # Find which observations have the highest dfbetas
  top_vals <- df_betas[predictor] %>%
    arrange(desc(abs(eval(parse(text = predictor))))) %>%
    .[1:nLabels,] %>%
    pull(predictor)
  top_ind <- which(pull(df_betas, predictor) %in% top_vals)

  out <- ggplot() +
    geom_point(data = df_betas,
               mapping = aes(x = as.numeric(rownames(df_betas)),
                             y = abs(pull(df_betas, predictor)))) +
    geom_text(mapping = aes(x = top_ind,
                            y = abs(pull(df_betas, predictor)[top_ind]) + 0.07,
                            label = top_ind)) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ylab("Abs of DFBETAS") +
    xlab("Observation Number") +
    ggtitle(predictor)

  if(length(dfbetas) <= 30) {
    out <- out +
      geom_hline(mapping = aes(yintercept = 1),
                 color = "red", linetype = "dashed")
  }else {
    out <- out +
      geom_hline(mapping = aes(yintercept = 2 / sqrt(length(dfbetas))),
                 color = "red", linetype = "dashed")
  }
  return(out)
}

#' Matrix of DFBETAS Plots
#'
#' For each predictor variable, plots the DFBETAS against observation number.
#'
#' @param model A linear regression model of class `stats::lm`
#' @param nLabels The number of potential influential points to label. Defaults
#' to 3. A value of 0 means do not label potential influential points.
#'
#' @export
jcreg_dfbetas <- function(model, nLabels = 3) {
  predictors <- attr(model$terms, "term.labels")
  df_betas <- as_tibble(dfbetas(model)[, predictors])

  plots <- lapply(predictors, dfb_col, df_betas = df_betas)
  plots["ncol"] <- ceiling(sqrt(length(plots)))
  do.call(grid.arrange, plots)
}

#' DFFITS Plot
#'
#' Plots the DFFITS against observation number.
#'
#' @param model A linear regression model of class `stats::lm`
#' @param nLabels The number of potential influential points to label. Defaults
#' to 3. A value of 0 means do not label potential influential points.
#'
#' @export
jcreg_dffits <- function(model, nLabels = 3) {
  df_fits <- dffits(model)
  top_dff <- as.numeric(names(sort(abs(df_fits), decreasing = TRUE)[1:nLabels]))

  df_fits_plot <- ggplot() +
    geom_point(data = tibble(df_fits),
               mapping = aes(x = as.numeric(names(df_fits)),
                             y = abs(df_fits))) +
    geom_text(mapping = aes(x = top_dff,
                            y = abs(df_fits[top_dff]) + max(df_fits) / 40,
                            label = top_cd)) +
    theme_bw() +
    ylab("Absolute Value of DFFITS for Y") +
    xlab("Observation Number") +
    theme(aspect.ratio = 1)
  if(length(df_fits) <= 30) {
    df_fits_plot +
      geom_hline(mapping = aes(yintercept =
                                 2 * sqrt(length(model$coefficients) /
                                            length(df_fits))),
                 color = "red", linetype = "dashed")
  }else {
    df_fits_plot +
      geom_hline(mapping = aes(yintercept = 1),
                 color = "red", linetype = "dashed")
  }
}
