# Installing will required packages

# library(utils)
# library(dplyr)
# library(mice)
# library(lime)
# library(keras)
# library(ggplot2)
# library(ggthemes)
# library(keras)
# library(tidyquant)
# library(rsample)
# library(recipes)
# library(yardstick)
# library(corrr)
# library(forcats)
# library(dplyr)
# library(readr)
# library(tidyr)

### First specify the packages of interest
packages = c("tidyverse", "utils","mice","dplyr",'lime','keras','ggplot2','ggthemes','tidyquant','rsample','recipes','yardstick','corrr','readr','tidyr',
             "phytools", "viridis")

## Now load or install & load all
## if the packages are in the system or not; if not, go ahead with the installation
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

### Dynamic name for file
set.seed(007)
read_data <- function(x) {
  ### Will read data from input folder
  churn_data_raw <- read.csv(x)
  glimpse(churn_data_raw)
  return(churn_data_raw)
}

