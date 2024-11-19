packages <- c("mice", 
"psych",
"tableone",
"ggplot2", 
"readxl",
"pROC",
"car", 
"openxlsx",
"dplyr",
"broom",
"tidyr")

## Now load or install & load all if necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

