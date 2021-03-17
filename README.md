# jcreg
This package sprang from trying to avoid copying and pasting code repeatedly for my linear regression class. I wanted a more elegant way to check linear regression assumptions and run variable selction methods, so here it is. For now, all the code is in `R/assumption_checking.R`. There is documentation available for all the funtions via `help()`, but for your reference here is a sort description of each function in the package.<br>

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

## Installation
The `devtools` package provides a convenient function for installing packages from github. You can use it as follows:

```R
install.packages("devtools")
library(devtools)
install_github("https://github.com/mtnbikerjoshua/jcreg")
```

## Contributing
I welcome suggestions, comments, and contributions. If you find a bug or want to request a feature please open an issue. If you simply want to make suggestions or comments or want to contact me directly, please send me an email at <mtnbikerjoshua@gmail.com>. If you would like to contribute to the code or documentation, please fork the project and submit a pull request.
