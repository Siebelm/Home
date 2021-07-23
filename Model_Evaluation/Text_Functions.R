# Remove quotation marks
pasteNQ <- function(...) {
  r <- paste(...)
  cat(r,"\n")
}
pasteNQ0 <- function(...) {
  r <- paste0(...)
  cat(r,"\n")
}

# Proportions
propTab <- function(data, 
                    exclude=NULL,
                    useNA = "no",
                    dnn=NULL,
                    deparse.level=1,
                    digits=0) {
  round(
    table(data, exclude=exclude, useNA=useNA, dnn=dnn, deparse.level=deparse.level)
    / NROW(data)*100,
  digits)
}

# Clean Data
basicclean <- function(rawtext) {
  # Set to lowercase
  rawtext <- tolower(rawtext)
  
  # Remove contractions
  fix_contractions <- function(rawtext) {
    rawtext <- gsub("will not", "won't", rawtext)
    rawtext <- gsub("can't", "can not", rawtext)
    rawtext <- gsub("can not", "cannot", rawtext)
    rawtext <- gsub("shant", "shall not", rawtext)
    rawtext <- gsub("n't", " not", rawtext)
    rawtext <- gsub("'ll", " will", rawtext)
    rawtext <- gsub("'re", " are", rawtext)
    rawtext <- gsub("'ve", " have", rawtext)
    rawtext <- gsub("'m", " am", rawtext)
    rawtext <- gsub("'d", " would", rawtext)
    rawtext <- gsub("'ld", " would", rawtext)
    rawtext <- gsub("'ld", " would", rawtext)
    rawtext <- gsub("'s", "", rawtext)
    return(rawtext)
  }
  rawtext <- fix_contractions(rawtext)
  
  # Remove puncutation, numbers, and other none characters
  rawtext <- removePunctuation(rawtext)
  rawtext <- removeNumbers(rawtext)
  rawtext <- gsub("[^[:alnum:]///' ]", "", rawtext)
  rawtext <- gsub("[']", "", rawtext)
  # Remove blanks
  rawtext <- na_if(rawtext, "")
  rawtext <- na_if(rawtext, "NA")
  
  # Strip whitespace
  rawtext <- stripWhitespace(rawtext)
  
  return(rawtext)
}

# Remove stop words
rm_stopwords <- function(rawtext, keep=NULL) {
  
  # Remove stop words
  stopwords_custom <- stopwords::stopwords("en", source = "snowball")
  rawtext <- removeWords(rawtext, stopwords_custom)
  
  return(rawtext)
}


######################
# Spell Check Function
######################

pacman::p_load(hunspell)

#' Spell check tool with an "ignore" dictionary to not correct
#' 
#' `OPA_Spellcheck`allows the `Hunspell` package to spell check a corpus while 
#' ignoring military acronyms, military jargon, and civilian slang.
#' @param text The list or column of documents to be spell checked (must be character class).
#' @param dict The dictionary or list of words to have the spell check ignore
#' @param author Michael Siebel
#' @example 
#' OPA_dict <- c("DoD", "offbase") # "Ignore" dictionary
#' orig <- data.frame( ID = 1:2, Text = c("I loove the DoD", "Do you dreenk offbase?") ) # Corpus as data frame
#' orig[ , "Text"] <- as.character(orig[ , "Text"]) # must be character (not factor)
#' updated <- orig # initiate spell checked corpus
#' updated[, "Text"] <- OPA_Spellcheck(orig[ , "Text"], OPA_dict) # spell check
#' ## Compare original to updated corpus
#' orig[ , "Text"] 
#' updated[ , "Text"]

OPA_Spellcheck <- function(text, dict) {
  # Words to check for misspelling
  words <- hunspell(text, format = "text", ignore = dict)
  
  # Unique words only
  unique <- sort(unique(unlist(words)))
  
  # Replace original text with spell checked text
  checked_text <- text
  for(i in 1:length(unique)) {
    checked_text <- str_replace_all(checked_text, 
                                    paste0("\\b", unique[i], "\\b"), 
                                    hunspell_suggest(unique[i])[[1]][1])
  }
  
  return(checked_text)
}


####################
# Sentiment Function
####################

# Sentiment Scores
sent_scores <- function(df, CleanText, dictionary = "vader") {
  average <- function(x) {
    pos <- sum(x[x>0], na.rm = T)
    neg <- sum(x[x<0], na.rm = T) %>% abs()
    neu <- length(x[x==0])
    bal <- ( (pos-neg)/(pos+neg) )*100
    y <- ifelse(is.nan(bal),0,bal %>% as.numeric())
    return(y)
  }
  df$Sentiment_Score <- sapply(CleanText, 
                               function(x) getSentiment(x, dictionary = dictionary, 
                                                        score.type = average)) 
  df$Sentiment_Cross <- NA
  df$Sentiment_Cross <- ifelse(df$Sentiment_Score<0, "Negative", df$Sentiment_Cross)
  df$Sentiment_Cross <- ifelse(df$Sentiment_Score>0, "Positive", df$Sentiment_Cross)
  df$Sentiment_Cross <- ifelse(df$Sentiment_Score==0, "Neutral", df$Sentiment_Cross)
  
  return(df)
}


##################
# Animated Writing
##################

animated_func <- function(df, text, normalize = TRUE, wordcount = NULL, total = FALSE) {
  if (normalize==TRUE) {
    # count exclamation marks
    df[ , "Exclamation_Mark"] <- (str_count(text, "[!]") / 
      df[ , wordcount]) %>%
      replace_na(list(. = 0)) %>%
      as.data.frame()
    
    # count question marks
    df[ , "Question_Mark"] <- (str_count(text, "[?]") / 
      df[ , wordcount]) %>%
      replace_na(list(. = 0)) %>%
      as.data.frame()
    
    # sentence length
    df[ , "Declarative"] <- (str_count(text, "[.]") /
      df[ , wordcount]) %>%
      replace_na(list(. = 0)) %>%
      as.data.frame()
  } 
  else if (normalize==FALSE) {
    # count exclamation marks
    df[ , "Exclamation_Mark"] <- str_count(text, "[!]") %>%
      replace_na(list(. = 0))
    
    # count question marks
    df[ , "Question_Mark"] <- str_count(text, "[?]") %>%
      replace_na(list(. = 0))
  }
  else if (normalize==FALSE & total==TRUE) {
    # count exclamation marks and question marks
    df[ , "Animated"] <- df[ , "Exclamation_Mark"] + df[ , "Question_Mark"]
    
    # remove exclamation marks
    df[ , "Exclamation_Mark"] <- NULL
    
    # remove question marks
    df[ , "Question_Mark"] <- NULL
  }
  
  return(df)
}


##############################################
# Wordcloud2 conversion for RMarkdown Function
##############################################

# Remove JavaScript from WordClouds
library("EBImage")
embed_htmlwidget <- function(widget, rasterise = T) {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(rasterise || outputFormat == 'latex') {
    html.file = tempfile("tp",fileext=".html")
    png.file = tempfile("tp",fileext=".png")
    
    htmlwidgets::saveWidget(widget, html.file, selfcontained = FALSE)
    webshot::webshot(html.file, file = png.file,
                     vwidth = 700, vheight = 500, delay =10)
    img = EBImage::readImage(png.file)
    EBImage::display(img)
  } else {
    widget
  }
}


################
# Table Function
################

# Replace NaN's with Zeros
nan_func <- function(x) {
  x <- ifelse(is.nan(x), 0, x %>% as.numeric())
  return(x)
}

# Stat Functions
prop_f <- function(x, p, wgt = NULL) {
  
  Var <- x[p==1] %>% nan_func()
  if (!is.null(wgt)) {
    Wgt <- wgt[p==1]
  } else {
    Wgt <- p[p==1]
  }
  Prop <- sum(Var*Wgt, na.rm = T)/sum(Wgt, na.rm = T)*100 
  Prop <- round(Prop, 0) %>% nan_func()
  return ( Prop )
}
freq_f <- function(x, p) {
  Var <- x[p==1]
  Freq <- sum(Var, na.rm = T)
  Freq <- ifelse(is.nan(Freq),0,Freq %>% as.numeric())
  return ( Freq )
}

# Table Function
table_func <- function(df, table_title, topics_n, crosses_n, rows_n, 
                       col_lab, col_names, pop = NULL) {
  
  # Define Cell Populations
  ## Total population (r01)
  if (is.null(pop)) {
    r01 <- c()
    r01[1:nrow(df)] <- 1
  } else {
    r01 <- pop
  }    
  ## Topics as subpopulation
  for (i in 1:topics_n) {
    ## Define r02-r0#
    assign( paste0("r", str_pad(i+1, 2, pad = "0")), 
            ifelse(df$Topic==i,
                   ifelse(r01==1,1,0),
                   0) )
  }
  
  # Create cell populations by crossings lists
  CrossList <- list()
  RowList <- list()
  for (j in 1:crosses_n) {
    ## Create list of crossings
    CrossList[j] <- paste0("Cross", j) 
    
    for (i in 1:rows_n) {
      ## Create list of rows
      RowList[i] <- paste0("r", str_pad(i, 2, pad = "0")) 
      
    }
  }    
  
  # Initiate columns list
  cols <- list()
  for (j in 1:crosses_n) {
    ## Initiate column
    col <- c()
    
    for (i in 1:rows_n) {
      ## Add rows to column
      col[i] <- prop_f( get(CrossList[[j]]), get(RowList[[i]]) ) 
      
    }
    cols[[j]] <- assign(paste0("col", j), col)
    
  }
  # Convert to data frame
  cols <- as.data.frame(cols)
  
  # Count of total comments
  all_count <- sum(r01)
  top_count <- table(df[r01==1, "Topic"])
  
  # Initiate frequency column
  col_freq <- c(all_count)
  for (i in 1:topics_n) {
    ## Create frequency column
    col_freq[i+1] <- top_count[i]
  }
  
  # Max percentage point difference between topic columns
  col_diff <- list()
  for (i in 1:crosses_n) {
    ## Create frequency column
    col_diff[[i+1]] <- max(cols[2:rows_n, i]) - min(cols[2:rows_n, i])
  }
  col_diff[[1]] <- as.character("Topic Diff")
  col_diff[[crosses_n+2]] <- ""
  
  # Draft table          
  final_table <- as.matrix(cbind(col_lab, cols, col_freq))
  final_table <- rbind(final_table, col_diff) %>% as.data.frame()
  names(final_table) <- col_names
  
  
  # Return table
  return(final_table %>% 
           kable(caption=table_title, row.names=F, format="html") %>% 
           kable_styling(full_width = F, position = "left", font_size = 12) %>%
           add_header_above(c(" "=1, "Proportion"=crosses_n, "Total"=1)) %>%
           column_spec(1, bold = T) %>%
           row_spec(1:(rows_n+1), color="black") %>%
           pack_rows("Total", 1, 1, 
                     label_row_css = "background-color: #666; color: #fff;") %>%
           pack_rows("Topic", 2, rows_n, 
                     label_row_css = "background-color: #666; color: #fff;") %>%
           pack_rows("Max Percentage Point Difference", rows_n+1, rows_n+1, 
                     label_row_css = "background-color: #666; color: #fff;")
  )
}


################
# Graph Function
################

# Table Function
graph_func <- function(df, graph_title, topics_n, rows_n, 
                       col_lab, crossing, color = "darkgreen", pop = NULL) {
  
  # Define Cell Populations
  ## Total population (r01)
  if (is.null(pop)) {
    r01 <- c()
    r01[1:nrow(df)] <- 1
  } else {
    r01 <- pop
  }    
  ## Topics as subpopulation
  for (i in 1:topics_n) {
    ## Define r02-r0#
    assign( paste0("r", str_pad(i+1, 2, pad = "0")), 
            ifelse(df$Topic==i,
                   ifelse(r01==1,1,0),
                   0) )
  }
  
  # Create cell populations
  RowList <- list()
  
  for (i in 1:rows_n) {
    ## Create list of rows
    RowList[i] <- paste0("r", str_pad(i, 2, pad = "0")) 
    
  }    
  
  # Initiate column
  cols <- c()
  bar_colors <- c()
  for (i in 1:rows_n) {
    ## Add rows to column
    cols[i] <- prop_f( crossing, get(RowList[[i]]) )
  }
  
  # Draft table          
  final_table <- as.data.frame(cbind(Topics = col_lab, 
                                 `Proportion (%)` = cols) ) 
  final_table$`Proportion (%)` <- as.numeric(as.character(final_table$`Proportion (%)`))

  return(final_table %>% 
         ggplot(aes(y = `Proportion (%)`, x = Topics, fill = factor(ifelse(Topics=="Total","Total","Other")) )) +
         geom_bar(stat="identity", width=0.9) + 
         scale_fill_manual(values = c("Total"="grey", "Other"=color)) +
         coord_flip() +
         theme_minimal() +
         theme(axis.title.y=element_blank()) +
         ggtitle(graph_title) +
         guides(fill=FALSE)
  )
  
}



###############
# STM Top Words
###############

# Top Words
topWords <- function(model, n=20, K=K) {
  topics <- labelTopics(model, 1:K, n=n, frexweight = 0.5)
  frex_words <- t(topics$frex)
  
  # Top Words
  for (j in 1:K) {
    cat("<strong> Topic",j,"</strong>")
    cat("\n\n")
    print(paste(frex_words[1:n , j], sep="\t"))
    cat("\n\n")
  }
}



############
# STM Quotes
############

quotes <- function(model, text, K=K) {
  for (i in 1:K) {
     thought <- findThoughts(model, texts = text, 
                             n = 2, topics = i)$docs[[1]]
     cat("<strong> Topic", i, "</strong>", "\n\n")
     str_replace_all(cat("Ex" ,1, "-", substr(thought[1], 1, 300), "\n\n"),
                           "NULL","A ")
     cat("Ex", 2, "-", substr(thought[2], 1, 300), "\n\n\n")
  }
}


longQuotes <- function(model, text, K=K) {
  for (i in 1:K) {
    thought <- findThoughts(model, texts = text, 
                            n = 4, topics = i)$docs[[1]]                                          
     cat("<strong> Topic", i, "</strong>", "\n\n")
     str_replace_all(cat("Ex" ,1, "-", substr(thought[1], 1, 1000), "\n\n"),
                           "NULL","A ")
     cat("Ex", 2, "-", substr(thought[2], 1, 1000), "\n\n")
     cat("Ex", 3, "-", substr(thought[3], 1, 1000), "\n\n")
     cat("Ex", 4, "-", substr(thought[4], 1, 1000), "\n\n\n")
  }
}


##############
# Bag of Words
##############

# Function
bow_func <- function(train_text, test_text = NULL, val_text = NULL,
                     ngram = c(1, 1), doc_count_min = 1L, doc_count_max = Inf,
                     doc_proportion_min = 0, doc_proportion_max = 1, 
                     seed = 2020) {
  set.seed(seed)
  
  # Initiate results
  dtm_test <- c()
  dtm_val <- c()
  
  #Train
  ## IDs
  ids <- 1:length(train_text)
  
  ## tokenize
  it_train <- itoken(train_text, 
                     ids = ids, 
                     progressbar = FALSE) 
  
  ## ngrams
  vocab <- create_vocabulary(it_train, ngram) 
  vocab <- prune_vocabulary(vocab, 
                            doc_count_min = doc_count_min, 
                            doc_count_max = doc_count_max,
                            doc_proportion_min = doc_proportion_min, 
                            doc_proportion_max = doc_proportion_max)
  
  vectorizer <- vocab_vectorizer(vocab)
  
  ## create dtm
  dtm_train <- create_dtm(it_train, vectorizer, type="dgCMatrix")
  
  
  if (!is.null(test_text)) {
    # Test
    ## IDs
    ids <- 1:length(test_text)
    
    ## tokenize
    it_test <- itoken(test_text, 
                      ids = ids, 
                      progressbar = FALSE) 
    
    # create dtm
    dtm_test <- create_dtm(it_test, vectorizer, type="dgCMatrix")
    
  } else { dtm_test <- "No test data provided" }
  
  
  if (!is.null(val_text)) {
    # validation 
    ## IDs
    ids <- 1:length(val_text)
    
    ## tokenize
    it_top <- itoken(val_text, 
                     ids = ids, 
                     progressbar = FALSE) 
    
    # create dtm
    dtm_val <- create_dtm(it_top, vectorizer, type="dgCMatrix")
    
  } else { dtm_val <- "No validation data provided" }
  
  
  return(list(dtm_train, dtm_test, dtm_val))
}
