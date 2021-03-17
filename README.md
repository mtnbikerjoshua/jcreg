# jcreg
This package sprang from trying to avoid copying and pasting code repeatedly for my linear regression class. I wanted a more elegant way to check linear regression assumptions and run variable selction methods, so here it is. For now, all the code is in R/assumption_checking.R . There is documentation available for all the funtions via `help()`, but for your reference here is a sort description of each function in the package.<br>

| Function        | Description                                                      |
| --------------- | ---------------------------------------------------------------- |
| cor_graphic     | A nice graphic of the correlation matrix                         |
| jcreg_av        | Square added variable plots                                      |
| jcreg_boxplot   | Beautified boxplot of residuals                                  |
| jcreg_cooksd    | Plot absolute value of cooks distance against observation number |
| jcreg_dfbetas   | Matrix of DFBETAS plots against observation number               |
| jcreg_dffits    | Plot DFFITS against observation number                           |
| jcreg_hist      | Histogram of residuals overlaid with normal curve                |
| jcreg_qq        | Quantile-Quantile plot of residuals                              |
| point_matrix    | Square scatterplot matrix                                        |
| resid_vs_fitted | Square residuals vs fitted values plot                           |
| resid_vs_pred   | Matrix of residuals vs predictor plots                           |
| var_selection   | Run multiple variable selection methods and compare the results  |
