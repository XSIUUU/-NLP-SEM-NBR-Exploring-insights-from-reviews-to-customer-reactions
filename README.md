# From Words to Action: Modeling the Cognitive and Behavior Paths from Review Features to Consumer Engagement

This project implements a two-stage empirical analysis pipeline that explores how review content features and reviewer characteristics influence consumer engagement behavior on the Yelp platform. It integrates **Structural Equation Modeling (SEM)** and **Negative Binomial Regression (NBR)** using real-world data.

## 📚 Study Overview

- **Study 1 (SEM)**: Uses a sample of 5,000 Yelp restaurant reviews to test how *Argument Quality (AQ)* and *Source Credibility (SC)* predict *Review Usefulness (RU)*.
- **Study 2 (NBR)**: Uses the full dataset of 170,615 reviews to examine how *Valence*, *RU*, and their interaction affect *Engagement Behavior (EB)*.

## 🧪 Methodology

- Latent variables (AQ & SC) measured through NLP features:
  - AQ: clarity, word count, objectiveness, topic relevance, comprehensiveness
  - SC: reviewer reputation, elite years, membership length
- Valence and objectiveness derived using pre-trained BERT (`nlptown/bert-base-multilingual-uncased-sentiment`)
- Topic relevance calculated via LDA topic modeling using `topicmodels` and `ldatuning`
- Behavioral outcome: number of new reviews generated within 3 months after a focal review

## 🔧 Code Structure

All analyses were performed in **R** with extensive use of the following packages:
- Text processing: `quanteda`, `hunspell`, `topicmodels`, `ldatuning`, `tidytext`
- SEM modeling: `lavaan`, `semPlot`
- Regression modeling: `MASS`, `car`, `pscl`
- Python integration: `reticulate` + Hugging Face `transformers` (BERT sentiment)

Key steps:
```r
# Load & clean Yelp data
Review <- read.csv("yelp_academic_dataset_review_short.csv")
Business <- read.csv("yelp_academic_dataset_business_short.csv")
User <- read.csv("yelp_academic_dataset_user.csv")

# Merge & preprocess
# ... see `0.3 Create DV: New reviews per quarter` in code

# NLP: topic modeling for relevance
lda_model <- LDA(dtm_stem_sample, k = 17, method = "Gibbs")

# NLP: BERT sentiment & objectiveness
tokenizer <- transformers$AutoTokenizer$from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")

# SEM modeling
library(lavaan)
sem_model <- 'RU ~ AQ + SC'
fit <- sem(sem_model, data = sem_data)

# NBR modeling
model_nbr <- glm.nb(new_reviews_3m ~ valence * usefulness + controls, data = train_set)
```

## 📁 Files & Data

- `restaurant_review.csv`: Cleaned review-level dataset
- `review_dfm.rds`: Document-feature matrix for topic modeling
- `sample_topic.csv`: Reviews with assigned topics and relevance scores
- `sentiment_results.rds`: BERT-based sentiment scores for each review

## 📈 Output

- Validates that both AQ and SC significantly predict RU (Study 1)
- RU and valence significantly predict EB; their interaction is negative (Study 2)
- Code structure supports reproducibility with organized scripts and datasets
