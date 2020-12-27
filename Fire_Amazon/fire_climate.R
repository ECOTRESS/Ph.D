# Set theme and directory
#-------------------------
library(randomForest)
library(caTools)
library(tidyverse)
# Set theme and directory
#-------------------------
#setwd("D:/Trabalhando/IRVINE/data")

# Set theme 
#-------------------------
theme_set(
  theme_minimal() + 
    theme(legend.position = "right", legend.title = element_blank())
)

# Data
#-------------------------

df = read.csv("C:/Users/santoslr/Desktop/Lucas/phd/results/tables/fire_with_vpd_and_pr.csv")

df_adj_vpd = df %>%
  as_tibble() %>%
  spread(id,median) %>%
  filter(is.na(precipitation)) %>%
  select(-precipitation, - X)

df_adj_pr = df %>%
  as_tibble() %>%
  spread(id,median) %>%
  filter(!is.na(precipitation)) %>%
  select(-vpd, - X)

df_adj = merge(df_adj_vpd, df_adj_pr) %>%
  mutate(#size = as.factor(ifelse(Size > 2.5,1,0)),
         fire = as.factor(Fire),
         month = as.factor(month_f)) %>% # Only differenciate huge from small fires
  select( - fire_type,
          #- Size,
          - Fire, - month_f)

sapply(df_adj, class)

# Check data
#-------------------------

# distribution Continuos
df_adj %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot() + aes(value) + 
  geom_histogram() + 
  facet_wrap(~key, scales = 'free')

# distibution Discrete
df_adj %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot() + aes(value) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~key, scales = 'free_x')

# Relation plot
  # df_adj %>%
  # group_by(fire,month) %>%
  # select_if(is.numeric) %>%
  # summarise(across(everything(), list(min = min, max = max))) %>%
  # gather(key,value,-fire,-month) %>%  
  # ggplot() + aes(x = month  , y = value , color = key) +
  #  geom_point() + facet_wrap(~fire)


# Random Forest
#-------------------------
smp_siz = floor(0.75*nrow(df_adj))

train_ind = sample(seq_len(nrow(df_adj)),size = smp_siz)
train = df_adj[train_ind,] 
test= df_adj[-train_ind,] 

# Model traning (Random Forest)
df_adj_rf = randomForest(Size ~ .,
                         #ntree = 500,
                         data = train,
                         importance=TRUE)

# View Random Forest
print(df_adj_rf)

# Var importance
varImpPlot(df_adj_rf)

# Predict training set
preds = predict(df_adj_rf, train)

plotlim = range(train$Size)

plot(preds~train$Size,
     xlab = "Observed fire size [km2]",
     ylab = "Simulated fire size [km2]",
     ylim=plotlim,xlim=plotlim, main = "Predicted vs Observed fire size -- Training set")
abline(c(0,1))

# Predict training set
preds_test = predict(df_adj_rf, test)

plotlim = range(test$Size)

plot(preds_test~test$Size,
     xlab = "Observed fire size [km2]",
     ylab = "Simulated fire size [km2]",
     ylim=plotlim,xlim=plotlim, main = "Predicted vs Observed fire size -- Test set")
abline(c(0,1))

# Input predictors(raster)

# Random Forest per fire group
#-------------------------
# pred_groups = split(seq_len(nrow(df_adj)), df_adj$fire)
# 
# preds = numeric(nrow(df_adj))
# 
# for(f in names(pred_groups)) {
#   
#   
#   
#   train_rows = df_adj$month == f
#   
#   df_adj_rf = randomForest(Size ~ .,
#                            #ntree = 500,
#                            data = df_adj[train_rows, ],
#                            importance=TRUE)
#   
#   print(df_adj_rf)
#  
#   varImpPlot(df_adj_rf, main = f)
#   
#   pred_rows = pred_groups[[f]]
#    
#   preds[pred_rows] = predict(df_adj_rf,
#                              df_adj[pred_rows, ])
# }
