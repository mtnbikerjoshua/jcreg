#' Scatterplot matrix
#'
#' `point_matrix` returns a basic scatterplot matrix of the data input.
#'
#' This function uses pairs(data) to generate a scatterplot matrix with square
#' plots and solid points. Note that for best results you should set your plot
#' size to be square.
#'
#' @param data The coordinates of points given as numeric columns of a matrix or
#'   data frame. See parameter `x` in `pairs()` for more detail.
#' @param pch Point type passed on to `pairs`.
#'
#' @examples
#' point_matrix(bodyfat)
#' point_matrix(mtcars[ ,c("mpg", "disp", "hp", "drat")])
#'
#' @export
point_matrix <- function(data, pch = 19) {
  par(pty = "s", las = 1)
  pairs(data, pch = pch, lower.panel = NULL)
}

#' Correlation Plots
#'
#' `cor_graphic` uses the "corrplot" package to create a nice graphic with a
#' color-coded correlation matrix and correlation plot side-by-side. Note that
#' if plot dimensions are too short or too small, there will be overlapping.
#'
#' @param data A numeric vector, matrix or data frame to be used as input to
#'   `cor`
#' @param show_key If TRUE, will show a color key.
#' @param title If FALSE, will hide titles.
#'
#' @examples
#' cor_graphic(bodyfat)
#'
#' @export
cor_graphic <- function(data, show_key = FALSE, title = TRUE) {
  par(mfrow = c(1, 2))
  corrplot::corrplot(cor(data), method = "number", type = "upper", diag = F,
                     tl.col = "#1f3366", cl.pos = "n")
  if(title) title("Correlation Coefficients")
  if (show_key) {
    corrplot::corrplot(cor(data), type = "upper", diag = F, tl.col = "#1f3366")
  }
  else {
    corrplot::corrplot(cor(data), type = "upper", diag = F, tl.col = "#1f3366",
                       cl.pos = "n")
  }
  if(title) title("Correlation Matrix")
}

#' Residuals Vs Fitted Values Plot
#'
#' `resid_vs_fitted` uses `autoplot` from `ggfortify` to create a square
#' residuals vs fitted values plot with a nice theme
#'
#' @param model A linear regression model of class `stats::lm`.
#'
#' @return Returns a ggplot object
#'
#' @examples
#' resid_vs_fitted(lm(brozek ~ ., data = bodyfat))
#'
#' @export
resid_vs_fitted <- function(model) {
  require(ggfortify)
  autoplot(model, which = 1, ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(aspect.ratio = 1)
}

#' Residuals vs Predictor Plot
#'
#' A helper function for resid_vs_pred. Plots the residuals vs the predictor for
#' a single predictor variable. Returns a ggplot2::ggplot object.
#'
#' @param data A dataframe containing one or more columns including the
#'   predictor variable.
#' @param residuals A vector or a dataframe containing the residuals assosiated
#'   with the data. Its length must be the same as the numer of rows in `data`.
#' @param predictor A string. The name of the column in `data` to be used as the
#'   predictor variable.
rpred_col <- function(data, residuals, predictor) {
  ggplot2::ggplot(data = data,
         mapping = ggplot2::aes(x = pull(data, predictor),
                       y = residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, span = 0.95, n = 7, size = 0.5) +
    ggplot2::geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(aspect.ratio = 1) +
    ggplot2::xlab(predictor) +
    ggplot2::ylab("Residuals")
}

#' Matrix of Residuals Vs Predictor Plots
#'
#' `resid_vs_pred` uses ggplot2::ggplot to create a matrix of residual vs
#' predictor plots
#'
#' @param A linear regression model of class `stats::lm`.
#'
#' @return Returns a gridExtra object with class "gtable", "gTree", "grob", and
#'   "gDesc".
#'
#' @examples
#' resid_vs_pred(lm(brozek ~ ., data = bodyfat))
#'
#' @export
resid_vs_pred <- function(model) {
  data <- model.frame(model)
  predictors <- attr(model$terms, "term.labels")
  plots <- lapply(predictors, rpred_col, data = data, residuals = resid(model))
  plots["ncol"] <- ceiling(sqrt(length(plots)))
  plots["top"] <- "Residuals vs Predictors"
  do.call(gridExtra::grid.arrange, plots)
}

#' Added-Variable Plots
#'
#' Wrapper for `car::avPlots` with square plots and well laid out.
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @return Returns the output of car::avPlots
#'
#' @examples
#' jcreg_av(lm(brozek ~ ., data = bodyfat))
#'
#' @export
jcreg_av <- function(model) {
  predictors <- attr(model$terms, "term.labels")
  rows <- ifelse(length(predictors > 3),
                 floor(sqrt(length(predictors))),
                 1)
  cols <- ifelse(length(predictors > 3),
                 length(predictors) / rows,
                 3)
  par(pty = "s")
  car::avPlots(model, layout = c(rows, cols), pch = 19)
}

#' Boxplot of residuals
#'
#' Uses ggplot2::ggplot to create a nicely formatted box plot of the residuals
#' of a model
#'
#' @param model A linear regression model of class `stats::lm`.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#' jcreg_boxplot(lm(brozek ~ ., data = bodyfat))
#'
#' @export
jcreg_boxplot <- function(model) {
  residuals <- data.frame(residuals = resid(model))
  ggplot2::ggplot(data = residuals, mapping = ggplot2::aes(y = residuals)) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(mapping = ggplot2::aes(x = 0),
                 fun = mean, geom = "point",
                 shape = 4, size = 2, color = "darkred") +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio = 2,
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()) +
    #  scale_y_continuous(limits = c(-20000, 30000), breaks = seq(-20000, 30000, 10000)) +
    ggplot2::ylab("Residuals") +
    ggplot2::xlab("")
}

#' Histogram of Residuals
#'
#' Uses ggplot2::ggplot to create a nicely formatted histogram of the residuals of a
#' model overlaid with a normal curve.
#'
#' @param model A linear regression model of class `stats::lm`
#'
#' @return Returns a ggplot object.
#'
#' @examples
#' jcreg_hist(lm(brozek ~ ., data = bodyfat))
#'
#' @export
jcreg_hist <- function(model) {
  residuals <- data.frame(residuals = resid(model))
  ggplot2::ggplot(data = residuals, mapping = ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(binwidth = sd(residuals$residuals / 4), mapping = ggplot2::aes(y = ..density..)) +
    ggplot2::stat_function(fun = dnorm,
                  color = "blue",
                  args = list(mean = 0,
                              sd = sd(residuals$residuals)),
                  size = 1.2) +
    ggplot2::xlab("Residuals") +
    ggplot2::ylab("Density") +
    ggplot2::theme_light()
}

#' Quantile - Quantile Plot
#'
#' Uses `autoplot` from `ggfortify` to plot a square, nicely-formatted Q-Q Plot
#' of the residuals of a linear model.
#'
#' @param model A linear regression model of class `stats::lm`.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#' jcreg_qq(lm(brozek ~ ., data = bodyfat))
#'
#' @export
jcreg_qq <- function(model) {
  require(ggfortify)
  autoplot(model, which = 2, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)
}

#' Cook's Distance Plot
#'
#' Plots the absolute value of cook's distance against observation number.
#'
#' @param model A linear regression model of class `stats::lm`.
#' @param nLabels The number of potential influential points to label. Defaults
#'   to 3. A value of 0 means do not label potential influential points. There
#'   is currently a bug where it always labels at least one point.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#' jcreg_cooksd(lm(brozek ~ ., data = bodyfat))
#'
#' @export
jcreg_cooksd <- function(model, nLabels = 3) {
  cooks_d <- cooks.distance(model)
  top_cd <- as.numeric(names(sort(cooks_d, decreasing = TRUE)[1:nLabels]))

  ggplot2::ggplot() +
    ggplot2::geom_point(data = tibble::tibble(cooks_d),
               mapping = ggplot2::aes(x = as.numeric(names(cooks_d)),
                             y = cooks_d)) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = top_cd,
                            y = cooks_d[top_cd] + max(cooks_d) / 40,
                            label = top_cd)) +
    ggplot2::theme_bw() +
    ggplot2::ylab("Cook's Distance") +
    ggplot2::xlab("Observation Number") +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 4 / length(cooks_d)),
               color = "red", linetype = "dashed") +
    ggplot2::theme(aspect.ratio = 1)
}

#' DFBETAS Plot
#'
#' A helper function for `jcreg_dfbetas`. Plots the DFBETAS for a single
#' predictor variable. Returns a ggplot2::ggplot object.
#'
#' @param df_betas A dataframe containing one or more columns including the
#'   DFBETAS of the predict variable.
#' @param residuals A vector or a dataframe containing the residuals assosiated
#'   with the data. Its length must be the same as the numer of rows in `data`
#' @param predictor A string. The name of the column in `data` to be used as the
#'   predictor variable.
#' @param nLabels The number of potential influential points to label. Defaults
#'   to 3. A value of 0 means do not label potential influential points. There
#'   is currently a bug where it always labels at least one point.
dfb_col <- function(df_betas, predictor, nLabels = 3) {
  require(tibble)
  # Find which observations have the highest dfbetas
  top_vals <- df_betas[predictor] %>%
    arrange(desc(abs(eval(parse(text = predictor))))) %>%
    .[1:nLabels,] %>%
    pull(predictor)
  top_ind <- which(pull(df_betas, predictor) %in% top_vals)

  out <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df_betas,
               mapping = ggplot2::aes(x = as.numeric(rownames(df_betas)),
                             y = abs(pull(df_betas, predictor)))) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = top_ind,
                            y = abs(pull(df_betas, predictor)[top_ind]) + 0.07,
                            label = top_ind)) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1) +
    ggplot2::ylab("Abs of DFBETAS") +
    ggplot2::xlab("Observation Number") +
    ggtitle(predictor)

  if(length(dfbetas) <= 30) {
    out <- out +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                 color = "red", linetype = "dashed")
  }else {
    out <- out +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 2 / sqrt(length(dfbetas))),
                 color = "red", linetype = "dashed")
  }
  return(out)
}

#' Matrix of DFBETAS Plots
#'
#' For each predictor variable, plots the DFBETAS against observation number.
#'
#' @param model A linear regression model of class `stats::lm`.
#' @param nLabels The number of potential influential points to label. Defaults
#'   to 3. A value of 0 means do not label potential influential points. There
#'   is currently a bug where it always labels at least one point.
#'
#' @return Returns a gridExtra object with class "gtable", "gTree", "grob", and
#'   "gDesc".
#'
#' @examples
#' jcreg_dfbetas(lm(brozek ~ ., data = bodyfat), nLabels = 2)
#'
#' @export
jcreg_dfbetas <- function(model, nLabels = 3) {
  predictors <- attr(model$terms, "term.labels")
  df_betas <-  tibble::as_tibble(dfbetas(model))[, predictors]

  plots <- lapply(predictors, dfb_col, df_betas = df_betas, nLabels = nLabels)
  plots["ncol"] <- ceiling(sqrt(length(plots)))
  do.call(gridExtra::grid.arrange, plots)
}

#' DFFITS Plot
#'
#' Plots the DFFITS against observation number.
#'
#' @param model A linear regression model of class `stats::lm`
#' @param nLabels The number of potential influential points to label. Defaults
#'   to 3. A value of 0 means do not label potential influential points. There
#'   is currently a bug where it always labels at least one point.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#' jcreg_dffits(lm(brozek ~ ., data = bodyfat), nLabels = 1)
#'
#' @export
jcreg_dffits <- function(model, nLabels = 3) {
  df_fits <- dffits(model)
  top_dff <- as.numeric(names(sort(abs(df_fits), decreasing = TRUE)[1:nLabels]))

  df_fits_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data =tibble::tibble(df_fits),
               mapping = ggplot2::aes(x = as.numeric(names(df_fits)),
                             y = abs(df_fits))) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = top_dff,
                            y = abs(df_fits[top_dff]) + max(df_fits) / 40,
                            label = top_dff)) +
    ggplot2::theme_bw() +
    ggplot2::ylab("Absolute Value of DFFITS for Y") +
    ggplot2::xlab("Observation Number") +
    ggplot2::theme(aspect.ratio = 1)
  if(length(df_fits) <= 30) {
    df_fits_plot +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept =
                                 2 * sqrt(length(model$coefficients) /
                                            length(df_fits))),
                 color = "red", linetype = "dashed")
  }else {
    df_fits_plot +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                 color = "red", linetype = "dashed")
  }
}

#' Variable Selection
#'
#' Uses various stepwise selection and shrinkage methods to perform variable
#' selection for a linear model.
#'
#' @param data A data.frame or object coercible to a data.frame whose **last**
#'   column is the response variable and whose other columns are the predictor
#'   variables.
#' @param method A vector of variable selection methods. Options are
#'   "best_subsets", "forward", "backward", "seqrep", "lasso", "elastic".
#' @param metric The metric to use for stepwise selection methods. Passed on to
#'   `IC` in `bestglm::bestglm`. Options are "AIC", "BIC", "BICg", "BICq",
#'   "LOOCV", "CV".
#' @param type.measure Loss to use for cross-validation in shrinkage methods.
#'   Passed on to `type.measure` in `glmnet::cv.glmnet`. Options are "default",
#'   "mse", "deviance", "class", "auc", "mae".
#' @param row Which way to orient the table of results. By default puts methods
#'   as rows and variables as columns. Can also be "var" which puts variables as
#'   rows and methods and columns.
#' @param lambda For shrinkage methods, use "lambda.1se" or "lambda.min".
#'
#' @return Output is a list of class `var_selection` and includes a table of
#'   booleans indicating which variables were included in the best model from
#'   each selection method, a list containing the best model from each selection
#'   method and the output of each selection method. Printing this object will
#'   provide a table of the results. Also note that, for the shrinkage methods,
#'   there is a plot of loss against lambda saved in the method object eg.
#'   `my_var_selection$lasso$plot`. For a more detailed summary of the output you
#'   will soon be able to use `summary`. That feature is in development.
#'
#' @examples
#' # Make sure to put the predictor variable last
#' bodyfat_reordered <- bodyfat[c("age", "weight", "height", "neck",
#'                                "chest", "abdom","brozek")]
#' var_selection(bodyfat_reordered)
#' var_selection(bodyfat_reordered,
#'               method = c("best_subsets", "seqrep", "elastic"),
#'               metric = "AIC")
#' var_selection(bodyfat_reordered, row = "var")
#'
#' @export
var_selection <- function(data, method = "all", metric = "BIC",
                          type.measure = "default", row = "method",
                          lambda = "lambda.1se") {
  require(bestglm) # For step-wise selection
  require(glmnet) # For shrinkage methods
  require(ggfortify) # For autoplot of lambdas

  if(method[1] == "all") {
    method <- c("best_subsets", "forward", "backward",
                                    "seqrep", "lasso", "elastic")
  }
  data <- as.data.frame(data) # In case `data` is a tibble, etc.

  # `cv.glmnet` requires the predictors and response to be in separate matrices
  predictors_matrix <- as.matrix(data[-length(data)])
  response_matrix <- as.matrix(data[length(data)])

  # A list of best models from each method
  best_models <- vector(mode = "list", length = length(method))
  names(best_models) <- method

  if("best_subsets" %in% method) {
    best_subsets <- bestglm(data, IC = metric, method = "exhaustive")
    best_models$best_subsets <- best_subsets$BestModel
  }
  if("forward" %in% method) {
    forward <- bestglm(data, IC = metric, method = "forward")
    best_models$forward <- forward$BestModel
  }
  if("backward" %in% method) {
    backward <- bestglm(data, IC = metric, method = "backward")
    best_models$backward <- backward$BestModel
  }
  if("seqrep" %in% method) {
    seqrep <- bestglm(data, IC = metric, method = "seqrep")
    best_models$seqrep <- seqrep$BestModel
  }
  if("lasso" %in% method) {
    lasso <- cv.glmnet(x = predictors_matrix, y = response_matrix,
                       type.measure = type.measure, alpha = 1)
    lasso$plot <- autoplot(lasso, label = FALSE) +
      theme_bw() +
      theme(aspect.ratio = 1)
    best_models$lasso <- coef(lasso, s = lambda)
  }
  if("elastic" %in% method) {
    elastic <- cv.glmnet(x = predictors_matrix, y = response_matrix,
                       type.measure = type.measure, alpha = 0.5)
    elastic$plot <- autoplot(elastic, label = FALSE) +
      theme_bw() +
      theme(aspect.ratio = 1)
    best_models$elastic <- coef(elastic, s = lambda)
  }
  if(row == "method") {
    # Create a matrix to be filled with booleans indicating which variables are
    #  included in the best model found by each selection method
    # For `row = "method"` put the methods as rows and the variables as columns
    method_names <- c("Best Subset", "Forward", "Backward",
                      "Sequential Rep.", "LASSO", "Elastic Net")
    method_abbr <- c("best_subsets", "forward", "backward",
                     "seqrep", "lasso", "elastic")
    models_table <- matrix(ncol = ncol(predictors_matrix),
                           nrow = length(best_models))
    rownames(models_table) <- method_names[method_abbr %in% method]
    colnames(models_table) <- colnames(predictors_matrix)

    # Loop through the models and populate the matrix
    for(i in 1:length(best_models)) {
      model <- best_models[[i]]
      # For the shrinkage methods we saved a matrix of the coefficients
      if("dgCMatrix" %in% class(model)) {
        models_table[i, ] <-
          colnames(predictors_matrix) %in%
          rownames(model)[attr(model, "i") + 1] # +1 for 1 based indexing
      }
      # For the other methods we saved the best model as an lm object
      else {
        models_table[i, ] <-
          colnames(predictors_matrix) %in%
          names(model$coefficients)
      }
    }
  }
  else if(row == "var") {
    # Create a matrix to be filled with booleans indicating which variables are
    #  included in the best model found by each selection method
    # For `row = "var"` put the varaibles as rows and the methods as columns
    method_names <- c("Best Subset", "Forward", "Backward",
                      "Sequential Rep.", "LASSO", "Elastic Net")
    method_abbr <- c("best_subsets", "forward", "backward",
                     "seqrep", "lasso", "elastic")
    models_table <- matrix(nrow = ncol(predictors_matrix),
                           ncol = length(best_models))
    rownames(models_table) <- colnames(predictors_matrix)
    colnames(models_table) <- method_names[method_abbr %in% method]

    # Loop through the models and populate the matrix with the included variables
    for(i in 1:length(best_models)) {
      model <- best_models[[i]]
      # For the shrinkage methods we saved a matrix of the coefficients
      if("dgCMatrix" %in% class(model)) {
        models_table[, i] <-
          colnames(predictors_matrix) %in%
          rownames(model)[attr(model, "i") + 1] # +1 for 1 based indexing
      }
      # For the other methods we saved the best model as an lm object
      else {
        models_table[, i] <-
          colnames(predictors_matrix) %in%
          names(model$coefficients)
      }
    }
  }
  # The table is returned as a matrix of booleans, but will be printed with X's
  #  in place of TRUE's
  result <- list(table = models_table,
       best_models = best_models,
       best_subsets = best_subsets,
       forward = forward,
       backward = backward,
       seqrep = seqrep,
       lasso = lasso,
       elastic = elastic,
       metric = metric,
       type.measure = type.measure)
  class(result) <- "var_selection"
  return(result)
}

#' Print `var_selection` Object
#'
#' Implements a print function for an object of type `var_selection`
#'
#' @param obj An object of class `var_selection`
#'
#' @export
print.var_selection <- function(obj) {
  # Print the table of results using X's in place of TRUE's
  print(ifelse(obj$table, "X", ""), quote = FALSE)
  cat("\nStepwise Metric =", obj$metric, "\tShrinkage Measure =", obj$type.measure)
}
