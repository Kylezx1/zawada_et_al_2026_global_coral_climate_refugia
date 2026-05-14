# Default values for habitat modelling pipeline

# Override these by specifying them in the setup files

xgb_booster = 'gbtree'
global_eval_metric = 'rmse'
global_device = 'cuda'
global_nthread = NULL

tree_max_depth = 40   # Adjust as needed
tree_eta = 0.01     # Learning rate
tree_gamma = 1
tree_min_child_weight = 2
tree_sampling_method = 'gradient_based'
tree_subsample = 0.7
tree_colsample_bytree = 0.7
tree_lambda = 0.5
tree_max_bin = 256*2
tree_alpha = 0
tree_num_parallel_tree = 1
tree_method = 'hist'

linear_lambda_bias = NULL

training_rounds = 100000
early_stopping_rounds = 5000
print_progress = T
print_every_n = 1000

quantile_alpha = NULL

# SHAP Values
quick_shaps <- F
quick_n_thresh <- 10000


map_crop <- F


spat_auto_cor_max_dist <- 500
time_auto_cor_max_dist <- 55
