Methodology
Model Ensembling with Down Sampling

I)   Split 70% train, 15% test, 15% validation
II)  Convert training data to multiple datasets
     - census minority class on each dataset
     - randomly sample w/o replacement majority class on each dataset
     - create 50/50 balance on each dataset
     - num_dfs = ceiling( lcl / scl )
     - df_size = scl * 2; remainder df_size = lcl * 2
III) Run each model independently 
     - Focus hyperparameter tuning on converengce as opposed to overfitting
     - Data is too small to focus on overfitting
     - Separating models then ensembing will reduce overfitting
IV)  Ensemble predicted probabilities via averaging
V)   Determine classification threshold on testing data
     - test various classification thresholds 
     - center attempts with middle threshold based on this formula:
       * thrshld = scl / (scl + lcl)
VI)  Bag hyperparameters and use new threshold on validation data
     - redo modeling with bagged hyperparameters on training data
     - use threshold determined by testing data
     - determine model performance on validation data (ignore testing data)

