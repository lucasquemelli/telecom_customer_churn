setwd("/Users/lucasquemelli/Documents/repos/telecom_customer_churn")
#source ("/Users/lucasquemelli/Documents/repos/telecom_customer_churn/data_ingestion.R")
#source ("/Users/lucasquemelli/Documents/repos/telecom_customer_churn/exploratory_data_analysis.R")
#source ("/Users/lucasquemelli/Documents/repos/telecom_customer_churn//pre_processing.R")

set.seed(007)
churn_data_raw = read.csv("/Users/lucasquemelli/Documents/repos/telecom_customer_churn/data/churn.csv")

churn_data_raw

View(churn_data_raw)

### First specify the packages of interest
packages = c("tidyverse", "utils","mice","dplyr",'lime','keras','ggplot2','ggthemes','tidyquant','rsample','recipes','yardstick','corrr','readr','tidyr', "phytools", "viridis")

## Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library("tidyverse")
library("utils")
library("mice")
library("dplyr")
library("lime")
library("keras")
library("ggplot2")
library("ggthemes")
library("tidyquant")
library("rsample")
library("recipes")
library("yardstick")
library("corrr")
library("readr")
library("tidyr")
library("phytools")
library("viridis")

#install.packages("tensorflow")
library(tensorflow)
library(reticulate)
#path_to_python <- install_python()
#virtualenv_create("r-reticulate", python = path_to_python)
#install_tensorflow(envname = "r-reticulate")

library(tensorflow)

#install.packages("keras")
library(keras)
#install_keras(envname = "r-reticulate")

#install.packages("tibble")
library(tibble)

#install.packages("MLmetrics")
library(MLmetrics)
library(dplyr)

### Visualizing Numerical  Variables ###
plot_num_var <- function (churn_data_raw){
  churn_data_raw %>%
    gather(x, y, -Churn) %>%
    ggplot(aes(x = y, fill = Churn, color = Churn)) +
    facet_wrap(~ x, ncol = 3, scales = "free") +
    geom_density(alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "top") +
    scale_color_tableau() +
    scale_fill_tableau()
  
}

### Pre-Processing data
missing_data_snapshot <- function (churn_data_raw){
  churn_data <- churn_data_raw %>%
    select(-customerID)
  mice::md.pattern(churn_data, plot = FALSE)
}

### Dropping those missing values
preprocess_data <- function (churn_data_raw){
  churn_data <- churn_data_raw %>%
    select(-customerID)
  mice::md.pattern(churn_data, plot = FALSE)
  churn_data <- churn_data %>%
    drop_na()
  return(churn_data)
}

### Creating train data
train_data <- function(churn_data_raw, train_proportion){
  churn_data_tbl <- churn_data_raw %>%
    select(-customerID) %>%
    drop_na() %>%
    select(Churn, everything())
  glimpse(churn_data_tbl)
  
  # Split test/training sets
  train_test_split <- initial_split(churn_data_tbl, prop = train_proportion)
  train_test_split
  return(train_test_split)
}

### Visualizing Categorical Variables ###
plot_cat_var<- function (churn_data_raw){
  
  churn_data_raw %>%
    select(-customerID) %>%
    select_if(is.character) %>%
    select(Churn, everything()) %>%
    gather(x, y, gender:PaymentMethod) %>% #select columns from gender to PaymentMethod
    count(Churn, x, y) %>%
    ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
    facet_wrap(~ x, ncol = 4, scales = "free") +
    geom_bar(stat = "identity", alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 9, hjust = 1),
          legend.position = "top") +
    scale_color_tableau() +
    scale_fill_tableau()
}

# Create recipe for performing some pre-processsing steps on the data
create_recipe <- function (train_tbl){
  rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
    step_discretize(tenure, options = list(cuts = 6)) %>%
    step_log(TotalCharges) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train_tbl)
  return(rec_obj)
}

### Selecting Numerical variables to visualize
num_variables <- c("Churn", "MonthlyCharges", "tenure", "TotalCharges")
num_var_data  <-  churn_data_raw[num_variables]

### Bi-variate analysis  

#Visualize the numerical data
plot_num_var(num_var_data)

#Visualize the categorical data
cat_var_data=churn_data_raw[,sapply(churn_data_raw,is.character) ]
plot_cat_var(cat_var_data)

# Missing values snapshot
missing_data_snapshot(churn_data_raw)

# Missing values removal
x <- preprocess_data(churn_data_raw)

# Percentage of missing values
100*11/7032

### Creating train train split with 80% data in train
train_test_split <- train_data(churn_data_raw, .8) ### Input data & % of data for training

# Partition train and test dataset
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

### Creating Recipe Object
rec_obj<- create_recipe(train_tbl)
rec_obj

### Baking the recipes
# Predictors
x_train_tbl <- bake(rec_obj, new_data = as.data.frame(train_tbl)) %>% select(-Churn)
x_test_tbl  <- bake(rec_obj, new_data = test_tbl) %>% select(-Churn)
glimpse(x_train_tbl)

# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)
glimpse(y_train_vec)

### Creating sequential model using Keras
dev_keras_model<- function(x_train_tbl){
  model_keras <- keras_model_sequential()
  
  model_keras %>%
    layer_dense(units = 32, kernel_initializer = "uniform", activation = "relu",
                input_shape = ncol(x_train_tbl)) %>%
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 16, kernel_initializer = "uniform", activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 8, kernel_initializer = "uniform", activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 1,
                kernel_initializer = "uniform", activation = "sigmoid") %>%
    
    compile(
      optimizer = 'adamax',
      loss      = 'binary_crossentropy',
      metrics   = c("binary_accuracy", "mse")
    )
  
  return(model_keras)
  
}

#######  Building Keras Model#######
model_keras <- dev_keras_model(x_train_tbl)

### Viewing the model layers
model_keras

fit_model <- function(x_train_tbl, y_train_vec){
  history <- fit(
    object           = model_keras,
    x                = as.matrix(x_train_tbl),
    y                = y_train_vec,
    batch_size       = 32,
    epochs           = 35,
    validation_split = 0.30
    
  )
  
  return(history)
}

### Model Fitting & Performance
fit_model(x_train_tbl, y_train_vec)

### Predictions of the model
predict_estimates <- function(model_keras, x_test_tbl, y_test_vec){
  # Predicted Class
  yhat_keras_class_vec <- predict(object = model_keras, x = as.matrix(x_test_tbl)) %>% `>`(0.5) %>% k_cast("int32") %>%
    as.vector()
  #yhat_keras_class_vec <- model_keras %>% predict(x = as.matrix(x_test_tbl)) %>% `>`(0.5) %>% k_cast("int32") %>%
  #as.vector()
  
  # Predicted Class Probability
  yhat_keras_prob_vec  <- predict(object = model_keras, x = as.matrix(x_test_tbl)) %>% `>`(0.5) %>% k_cast("int32") %>%
    as.vector()
  
  # Format test data and predictions for yardstick metrics
  estimates_keras_tbl <- tibble(
    truth      = factor(y_test_vec, levels = c(0, 1)),
    estimate   = factor(yhat_keras_class_vec, levels = c(0, 1)),
    class_prob = yhat_keras_prob_vec 
    
  )
  
  estimates_keras_tbl
}

### Predictions from model on test data
estimates_keras_tbl <- predict_estimates(model_keras, x_test_tbl, y_test_vec)

### Judging model benchmark metrics
# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# AUC/ROC
estimates_keras_tbl %>% roc_auc(truth, class_prob)

# Recall for positive label
Recall(y_test_vec, ifelse(yhat_keras_class_vec > .5, 1, 0), positive = 1)

#F1-Score
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)

### Correlation Analysis ###
coorelation_analysis <- function (x_train_tbl,y_train_vec){
  
  
  # Feature correlations to Churn
  corrr_analysis <- x_train_tbl %>%
    mutate(Churn = y_train_vec) %>%
    correlate() %>%
    focus(Churn) %>%
    mutate(feature = colnames(x_train_tbl)) %>%
    arrange(abs(Churn)) %>%
    mutate(feature = as_factor(feature))
  corrr_analysis
  
  # Correlation visualization
  corrr_analysis %>%
    ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
    geom_point() +
    # Positive Correlations - Contribute to churn
    geom_segment(aes(xend = 0, yend = feature),
                 color = palette_light()[[2]],
                 data = corrr_analysis %>% filter(Churn > 0)) +
    geom_point(color = palette_light()[[2]],
               data = corrr_analysis %>% filter(Churn > 0)) +
    # Negative Correlations - Prevent churn
    geom_segment(aes(xend = 0, yend = feature),
                 color = palette_light()[[1]],
                 data = corrr_analysis %>% filter(Churn < 0)) +
    geom_point(color = palette_light()[[1]],
               data = corrr_analysis %>% filter(Churn < 0)) +
    # Vertical lines
    geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
    geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
    geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
    # Aesthetics
    theme_tq() +
    labs(title = "Churn Correlation Analysis",
         subtitle = paste("Positive Correlations (contribute to churn),",
                          "Negative Correlations (prevent churn)"),
         y = "Feature Importance")
}

### Correlation Insights
coorelation_analysis(x_train_tbl,y_train_vec)

# save the model in desired location
save_model_hdf5(model_keras, 'user_churn_model.hdf5')

# Load model
load_model_hdf5("/Users/lucasquemelli/Documents/repos/telecom_customer_churn/user_churn_model.hdf5", custom_objects = NULL, compile = TRUE)













