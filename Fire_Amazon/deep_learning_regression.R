# Deep Learning -- Regression 
# Learning via Keras API 

install.packages("tensorflow")
install.packages("keras")
install.packages("tfdatasets")
# Libs ------------------------
library(keras)
library(tfdatasets)
library(tidyverse)
install_tensorflow(method = 'conda', envname = 'r-reticulate')

# Data ------------------------

boston_housing = dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

# Check data ------------------------
paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))

# Display sample features, notice the different scales 
train_data[1, ]


# Set col names 
column_names = c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')

train_df = train_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = train_labels)

test_df = test_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = test_labels)

# Display first 10 entries (housing price $)
train_labels[1:10] 

# Normalize features
spec = feature_spec(train_df, label ~ . ) %>% 
  step_numeric_column(all_numeric(), 
                      normalizer_fn = scaler_standard()) %>% 
  fit()

spec

# output of a dense-features layer
layer = layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)
layer(train_df)


# Model through Keras API
input = layer_input_from_dataset(train_df %>% select(-label))

output = input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) 

model = keras_model(input, output)

summary(model)

# Compile model
model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )


# Wrap the model building code into a function
build_model = function() {
  input = layer_input_from_dataset(train_df %>% select(-label))
  
  output = input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model = keras_model(input, output)
  
  # Compile model
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback = callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

model = build_model()

history = model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history)


# Use this chunk if wants checking for improvements over epochs
# The patience parameter is the amount of epochs to check for improvement.
early_stop = callback_early_stopping(monitor = "val_loss",
                                      patience = 20)

model = build_model()

history = model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop)
)

plot(history) # Check there is an error when ploting


# Evaluat model in training set -- mae (mean absolute error)
c(loss, mae) %<-% (model %>% evaluate(test_df %>% select(-label), 
                                      test_df$label, verbose = 0))

paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

# Predict
test_predictions = model %>% predict(test_df %>% select(-label))
test_predictions[ , 1]

# Visualize predictions vs test set 
comp = cbind(test_df,test_predictions)

comp %>% 
  ggplot() + aes(x = comp$test_predictions, y = comp$label) +
  geom_point() + geom_smooth(method = "lm", se = F)
