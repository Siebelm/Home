Methodology
Model Ensembling with NLP Techniques

Goal:  Classify portions of text into the book that it belongs.  Then run film scripts through the model to determine which film is closest to its corresponding book.
I)   Define documents
     - Description: Create documents at the page-level
     - Purpose: Define portions of text that are small enough to provide many examples for the model but large enough to capture meaningful differences in text per book
            * page is defined as 250 words
            * series has 4,347 pages
            * split documents 70% train, 30% test
II)  Structure text
     - Description: Build 4 models using different NLP techniques
     - Purpose: Use multiple NLP techniques in order to take advantage of each of their strengths
III) Oversample
     - Description: Balance classes by oversampling from shorter pieces of text from training pages
     - Purpose: Enrich training data to improve predictions of shorter books
            * double training size and balance classes in training data
            * doubled training size helps reduce overpredictions of shorter books caused by oversampling
            * randomly sample single and triple paragraphs from training pages
            * use different samples per model by setting different seeds per sample
IV)  Run 4 models
     - Description: Run 4 models independently with hyper-parameter tuning
     - Purpose: Optimize 4 models
            * Test 25 different versions of hyper-parameters on 4-fold cross validation
            * Take best hyper-parameters and rerun on all training data 
            * Repeat for each model
V)   Perform stacked ensemble modeling
     - Description: Ensemble 4 bottom layer models with top layer model
     - Purpose: Take strengths of each model and minimize each model's weaknesses     
            * Resample training data for each 4 models with same seed 
            * Use 4 training models to generate predicted probabilities and save as seven columns per model in a new data frame (28 columns in total)
            * Perform PCA data reduction to convert 28 columns into 7
            * Model principal components with top layer logistic regression to priviledge model strengths
VI)  Determine final model performance
     - Description: Test results of stacked model ensemble on testing data
     - Purpose: Ensure model process and outcome is generalizable
VII) Implement model on film scripts
     - Description: Run final model on film scripts to generate predicted probability of matching book counterpart
     - Purpose: Answer research question
     
     
## Progress
```{r}
# Page Level
pages$Progress <- row.names(pages) %>% as.numeric() / nrow(pages) * 100

# Paragraph Level
para3$Progress <- row.names(para3) %>% as.numeric() / nrow(para3) * 100
para1$Progress <- row.names(para1) %>% as.numeric() / nrow(para1) * 100
```
# count caps-lock words
# pages$Caps_Lock <- str_count(pages$Text, "\\b[A-Z]{2,}$\\b") %>%
#                    ifelse(is.na(.),0,.)
# para3$Caps_Lock <- str_count(para3$Text, "\\b[A-Z]{2,}$\\b") %>%
#                    ifelse(is.na(.),0,.)
# para1$Caps_Lock <- str_count(para1$Text, "\\b[A-Z]{2,}$\\b") %>%
#                    ifelse(is.na(.),0,.)


# Word Embeddings (IDF) Model
## Bottom Layer
# set.seed(24)
# up_train <- upsample_train()
# we_idf_train_target <- up_train$Book
# we_idf_dtm <- dtm_we(up_train$Text, test$Text, 
#                      up_top_train$Text, doc2vec = "idf")
# we_idf_train <- we_idf_dtm[[1]]
# we_idf_test  <- we_idf_dtm[[2]]
# ## Top Layer
# we_idf_top_train <- we_idf_dtm[[3]]


# Word Embeddings (IDF) model
num_train <- as.factor(we_idf_train_target) %>% as.numeric() - 1
we_idf_train <- xgb.DMatrix(we_idf_train,
                            label=num_train)
we_idf_test  <- xgb.DMatrix(we_idf_test,
                            label=num_test)
                            
# save(target_test, train, test, train_nrow,
#      sent_train_target, sent_train, sent_test, 
#      bow_train_target, bow_train, bow_test,
#      bowtfidf_train_target, bowtfidf_train, bowtfidf_test, tfidf,
#      we_ss_train_target, we_ss_train, we_ss_test,
#      we_idf_train_target, we_idf_train, we_idf_test,
#      top_train_target, top_num_train,
#      results_test, num_test, 
#      file = "Harry_Potter_and_the_Classification.RData")

## Word Embeddings (IDF) Model
```{}
# cross validation
cv_best <- cv_tune(data = we_idf_train)
pasteNQ("Best NRound:")
cv_best[[1]] %>% as.numeric()
pasteNQ("Best Params:")
cv_best[[2]]

# best model
we_idf_model <- xgboost(data=we_idf_train, verbose=F,
                    nrounds=cv_best[[1]], param=cv_best[[2]])

# prediction
cols <- 29:35
pred_test[ , cols] <- predict(we_idf_model, we_idf_test) %>% 
                     matrix(ncol=num_class, byrow=TRUE)
results_test[ , "WE_IDF_Label"] <- max.col(pred_test[ , cols])

# error and accuracy measure
results_test[ , "WE_IDF_Label"] <- factor(results_test[ , "WE_IDF_Label"],
                                  labels = unique(results_test$Target))
we_idf_acc <- confusionMatrix(results_test$WE_IDF_Label, 
                          results_test$Target)
round( we_idf_acc$overall["Accuracy"], 2 )

# Graph
we_idf_acc$byClass %>% 
  as.data.frame() %>%
  ggplot(aes(x = `Balanced Accuracy` %>% as.numeric()*100, 
             y = titles, fill = titles)) +
  geom_bar(stat = "identity") +
  xlim(0, 100) +
  labs(title="Balanced Accuracy",
       y="Book",
       x="Balanced Accuracy (%)") + 
  scale_fill_manual(values = c("#946B2D", "#0D6217", "#000A90", 
                               "#AAAAAA", "#000000", "#7F0909", "#FFC500")) +
  Grph_theme_facet()
```

# best_eval <- cv_best[[3]]
# ggplot(best_eval) + 
#     geom_line(aes(y = test_mlogloss_mean, 
#                   x=row.names(best_eval) %>% as.numeric())) +
#     ggtitle("Test Data Logloss Means") +
#     labs(y = "Logloss", x = "Iteration") +
#     Grph_theme_facet()      


# Word Embeddings (IDF) model
# we_idf_top_train <- xgb.DMatrix(we_idf_top_train,
#                                 label=num_train)

# pred_train[ , 29:35] <- predict(we_idf_model, we_idf_top_train) %>% 
#                                 matrix(ncol=num_class, byrow=TRUE) 

# Name columns
# colnames(pred_train) <- c(paste0("Sent_Pred_", unique(results_test$Target)),
#                           paste0("BoW_Pred_", unique(results_test$Target)),
#                           paste0("BoWtfidf_Pred_", 
#                                  unique(results_test$Target)),
#                           paste0("WE_SS_Pred_", unique(results_test$Target)),
#                           paste0("WE_IDF_Pred_", unique(results_test$Target)))
# colnames(pred_test) <- c(paste0("Sent_Pred_", unique(results_test$Target)),
#                          paste0("BoW_Pred_", unique(results_test$Target)),
#                          paste0("BoWtfidf_Pred_", 
#                                 unique(results_test$Target)),
#                          paste0("WE_SS_Pred_", unique(results_test$Target)),
#                          paste0("WE_IDF_Pred_", unique(results_test$Target)))

# PCA
# pca <- irlba::prcomp_irlba(pred_train, n = 7, scale. = T)
# pca_train <- pca$x
# pca_test <- predict(pca, pred_test)

## Top Layer (GBDT)
```{r}
# Top layer model
top_gbdt_train <- xgb.DMatrix(as.matrix(pca_train),
                              label=num_train,
                              missing=NaN)
top_gbdt_test  <- xgb.DMatrix(as.matrix(pca_test),
                              label=num_test,
                              missing=NaN)

# Results
results_top_gbdt_test <- data.frame(matrix(nrow=nrow(test), ncol=7))

# cross validation
cv_best <- cv_tune(data = top_gbdt_train)
pasteNQ("Best NRound:")
cv_best[[1]] %>% as.numeric()
pasteNQ("Best Params:")
cv_best[[2]]

# best model
model_top_gbdt <- xgboost(data=top_gbdt_train, verbose=F,
                          nrounds=cv_best[[1]], param=cv_best[[2]])

# prediction
results_top_gbdt_test[ , 1:7] <- predict(model_top_gbdt, top_gbdt_test) %>% 
                                         matrix(ncol=num_class, byrow=TRUE)
results_top_gbdt_test[ , "Top_Label"] <- max.col(results_top_gbdt_test[ , 1:7])

# error and accuracy measure
results_test[ , "Top_Label"] <- factor(results_top_gbdt_test[ , "Top_Label"],
                                       labels = unique(results_test$Target))
top_gbdt_acc <- confusionMatrix(results_test$Top_Label, 
                                results_test$Target)
top_gbdt_acc

# Graph
top_gbdt_acc$byClass %>% 
  as.data.frame() %>%
  ggplot(aes(x = `Balanced Accuracy` %>% as.numeric()*100, 
             y = titles, fill = titles)) +
  geom_bar(stat = "identity") +
  xlim(0, 100) +
  labs(title="Balanced Accuracy",
       y="Book",
       x="Balanced Accuracy (%)") + 
  scale_fill_manual(values = c("#946B2D", "#0D6217", "#000A90", 
                               "#AAAAAA", "#000000", "#7F0909", "#FFC500")) +
  Grph_theme_facet()
```

# pasteNQ("Word Embedding (IDF) Model")
# we_idf_acc$overall["Accuracy"]
# cat("\n")

pasteNQ("Top Layer (GBDT) Model")
top_gbdt_acc$overall["Accuracy"]
cat("\n")

     pca_train, pca_test,
     we_idf_train, we_idf_test, we_idf_top_train, we_idf_model,
     model_top_gbdt, top_gbdt_test,
we_idf_acc,
     top_gbdt_acc, 