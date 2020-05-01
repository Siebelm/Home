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
     - Description: Build 5 models using different NLP techniques
     - Purpose: Use multiple NLP techniques in order to take advantage of each of their strengths
III) Oversample
     - Description: Balance classes by oversampling from shorter pieces of text from training pages
     - Purpose: Enrich training data to improve predictions of shorter books
            * double training size and balance classes in training data
            * doubled training size helps reduce overpredictions of shorter books caused by oversampling
            * randomly sample single and triple paragraphs from training pages
            * use different samples per model by setting different seeds per sample
IV)  Run 5 models
     - Description: Run 5 models independently with hyper-parameter tuning
     - Purpose: Optimize 5 models
            * Test 25 different versions of hyper-parameters on 4-fold cross validation
            * Take best hyper-parameters and rerun on all training data 
            * Repeat for each model
V)   Perform stacked ensemble modeling
     - Description: Ensemble 5 bottom layer models with top layer model
     - Purpose: Take strengths of each model and minimize each model's weaknesses     
            * Resample training data for each 5 models with same seed 
            * Use 5 training models to generate predicted probabilities and save as seven columns per model in a new data frame (35 columns in total)
            * Perform PCA data reduction to convert 35 columns into 7
            * Model principal components with top layer logistic regression to priviledge model strengths
VI)  Determine final model performance
     - Description: Test results of stacked model ensemble on testing data
     - Purpose: Ensure model process and outcome is generalizable
VII) Implement model on film scripts
     - Description: Run final model on film scripts to generate predicted probability of matching book counterpart
     - Purpose: Answer research question
     
     
     