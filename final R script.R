#######
# thesis preperation 
#######

#######
# Week 1 
#######

rm(list = ls()) #clear workspace

setwd("E:/onedrive/Groningen(online)/MADS/thesis/assignment 1") #

Review <- read.csv("yelp_academic_dataset_review_short.csv", header = TRUE)
Business <- read.csv("yelp_academic_dataset_business_short.csv", header = TRUE)
User <- read.csv("yelp_academic_dataset_user.csv", header = TRUE)

head(Review)
head(Business)
head(User)

################
# 0️⃣ data cleaning -----
################
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(data.table)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stopwords)
library(wordcloud2)
library(topicmodels)
library(ldatuning)
library(tidytext)
library(hunspell)
library(corrplot)
library(reticulate)
library(lavaan)
library(semPlot)
library(fastDummies)
library(pscl)
library(MASS)
library(car)
library(Metrics)

# Exploration
summary(Business)
colSums(is.na(Business))

# checking time scale
min(Review$date)
max(Review$date)

## 0.1 NAs & empty string removing ----
# It looks like there's missing but there's not, probably because of the "" empty string.
sum(Business == "", na.rm = TRUE)

# Replace empty string with na
Business <- Business %>%
  mutate(across(everything(), ~na_if(trimws(as.character(.)), "")))
summary(Business)
colSums(is.na(Business))	 # business的某些餐厅没有categories要去掉,别的na可以不管(后面直接filter选了餐厅)

Review <- Review %>%
  mutate(across(everything(), ~na_if(trimws(as.character(.)), "")))
colSums(is.na(Review))     # review完全没有空string或者NAs

Review <- Review %>% arrange(desc(date))
summary(Review)

## 0.2 Data type transformation ----
Review_clean <- Review %>% mutate(
  across(c(stars,useful,funny,cool),~ as.numeric(.x))) %>% 
  mutate(date = as.Date(date))

Business_clean <- Business %>% mutate(
  across(c(postal_code,latitude,longitude,stars,review_count),~ as.numeric(.x))) %>% 
  rename(business_stars = stars) 

User_clean <- User %>% select(user_id,yelping_since,useful,elite,friends,fans,average_stars) %>% 
  mutate(yelping_since = as.Date(yelping_since),
         elite_years = ifelse(elite == "", 0, str_count(elite, ",") + 1),
         friend_count = ifelse(friends == "", 0, str_count(friends, ",") + 1)) %>% 
  rename(user_avg_useful = useful)

colSums(is.na(Review_clean))
colSums(is.na(Business_clean))
colSums(is.na(User_clean))

## 0.3 create DV (new review per quarter)
Review_clean <- Review_clean %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = as.numeric(format(as.Date(date), "%m")),
    quarter = case_when(
      month %in% 1:3 ~ "Q1",
      month %in% 4:6 ~ "Q2",
      month %in% 7:9 ~ "Q3",
      month %in% 10:12 ~ "Q4"
    ),
    year_quarter = paste0(year, "-", quarter)
  ) 


# create new column of the date 3 months after review
Review_clean <- Review_clean %>%
  mutate(date_plus_3m = date %m+% months(3))

# create new df for following calculation
review_growth <- Review_clean %>%
  select(review_id, business_id, date, date_plus_3m) %>% 
  arrange(business_id,date)


# 使用data.table计算每条评论后三个月的新增评论数
setDT(review_growth)
review_growth[, new_reviews_3m := 0]

# 对每个 business 单独处理
review_growth[, new_reviews_3m := {
  n_total <- .N
  res <- integer(n_total)
  for (i in 1:n_total) {
    t1 <- date[i]
    t2 <- date_plus_3m[i]
    # 查找该评论时间后的 3 个月内的评论数（不包含自身）
    res[i] <- sum(date > t1 & date <= t2)
  }
  res
}, by = business_id]

# merge回原表格中
Review_full <- Review_clean %>%
  left_join(select(review_growth, review_id, new_reviews_3m),
            by = "review_id")

# 筛选：仅保留评论日期在 2021-09-30（含）之前的记录
Review_full <- Review_full[Review_full$date <= as.Date("2021-09-30"), ]
# 确保 date_plus_3m 不超过数据最晚时间，比如 "2021-12-31"
Review_full <- Review_full[Review_full$date_plus_3m <= as.Date("2021-12-31"), ]

# 计算会员长度(发帖日和创号日只差)
Review_full$membership_length <- as.numeric(as.Date(Review_full$date)-as.Date(Review_full$yelping_since))

# 计算文本的字数
Review_full$word_count <- sapply(strsplit(Review_full$text, "\\s+"), length)
Review_full$word_count  <-  as.numeric(scale(Review_full$word_count, center = TRUE, scale = TRUE))

write.csv(Review_full,file = "Review_full.csv", row.names = FALSE)  # 刚生成完,后面的代码要换成这个
Review_full <- read.csv("Review_full.csv", header = TRUE)


## 0.4 data filtering & merging ----

# check the percentage of restaurant among all industry
Business_clean$contains_restaurant <- grepl("restaurant", Business_clean$categories, ignore.case = TRUE)
sum(Business_clean$contains_restaurant)/nrow(Business_clean)*100

# 筛选一下,让business那边只留下餐饮,创个新df,(值得注意,筛选完餐厅评论还有2m5个)
restaurant_business <- Business_clean %>% filter(contains_restaurant)

# 合并
restaurant_review <- restaurant_business %>% left_join(Review_full , by = "business_id")
colSums(is.na(restaurant_review))

# 检查餐厅的数据占所有数据的百分比
nrow(restaurant_review)/nrow(Review_clean) # 0.67,也就是总评论数的67%都是餐厅的评论

# 由于大部分 user 都没有详细信息,只保留有user信息的数据,因此使用 inner join (筛选后只剩下了17w评论)
restaurant_review <- restaurant_review %>% inner_join(User_clean , by = "user_id")
colSums(is.na(restaurant_review))

## 0.5 data exploration ----

# 可视化新增评论的分布
ggplot(restaurant_review, aes(new_reviews_3m))+
  geom_histogram(fill = "#800020", color = "white")+
  geom_text(stat = "bin", aes(label = ..count..), vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of New Reviews Within 3 Months Following Each Review",
       y = "Frequency",
       x = "Number of New Reviews")

colSums(is.na(restaurant_review))

# 看一下哪些餐厅没有评论吧
sum(is.na(restaurant_review$text))

# 看一下多少个用户给餐厅留言
sum(n_distinct(restaurant_review$user_id)) 

# 看一下用户留言的频率
restaurant_review %>% group_by(user_id) %>% slice(review_count) %>% 
  ggplot(aes(review_count))+
  geom_histogram(fill = "#800020", color = "white")+
  geom_text(stat = "bin", aes(label = ..count..), vjust = -0.5, size = 3)+
  labs(title = "Distribution of Reviews by Each User",
       y = "Frequency",
       x = "Number of Reviews")

# 检查每个州的评论情况并清洗
restaurant_review %>% group_by(state) %>% 
  summarize(review_count = n())

# 由于 state HI/MT/NC 各只有一个样本,因此删除这三个样本,并且删除这三列
# 需要删除的 state 名称
rare_states <- c("HI", "MT", "NC")

# 1. 删除 state 为 HI/MT/NC 的观测值（假设有原始 state 列）
restaurant_review <- restaurant_review[!(restaurant_review$state %in% rare_states), ]

write.csv(restaurant_review,file = "restaurant_review.csv", row.names = FALSE)
restaurant_review <- read.csv("restaurant_review.csv",header = TRUE)

#######################################################
# 2️⃣ NLP step1 --create dfm--------------------
#######################################################

# create corpus
review_corpus <- corpus(restaurant_review$text)
docnames(review_corpus) <- restaurant_review$review_id  

# tokenization
review_tokens <- tokens(review_corpus,
                        what = "word",
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_symbols = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE)

# remove stop words (确保不会有"of the restaurant"这种ngram)
review_tokens <- tokens_remove(review_tokens, stopwords("en"))

# create n-gram
review_tokens <- tokens_ngrams(review_tokens, n = 1:3)

# remove custom stop words
custom_stopwords <- c(
  "just", "one", "us", "also", "got", "came", "can", "well", "even", 
  "always", "first", "come", "wait", "never", "little", "went","definitely")

review_tokens <- tokens_remove(review_tokens, custom_stopwords)

# create another tokens with stemming
review_tokens_stem <- tokens_wordstem(review_tokens)

# create dfm
review_dfm <- dfm(review_tokens)
review_dfm_stem <- dfm(review_tokens_stem)

# 删除dfm中一个token都没有的观测值
kept_review_ids <- docnames(review_dfm)[rowSums(review_dfm) > 0]
kept_stem_review_ids <- docnames(review_dfm)[rowSums(review_dfm) > 0]
setequal(kept_review_ids, kept_stem_review_ids) # 检查到两个所筛选掉的数据都是一样的,后面只用一个就好了

write.csv(kept_review_ids,file = "kept_review_ids.csv", row.names = FALSE)
kept_review_ids <- read.csv("kept_review_ids.csv",header = TRUE)

review_dfm <- review_dfm[kept_review_ids, ]
review_dfm_stem <- review_dfm_stem[kept_review_ids, ]

# 保存dfm,方便后续直接使用
saveRDS(review_dfm, file = "review_dfm.rds")
saveRDS(review_dfm_stem, file = "review_dfm_stem.rds")
review_dfm <- readRDS("review_dfm.rds")
review_dfm_stem <- readRDS("review_dfm_stem.rds")

# 同步删除原始数据中的废观测值
restaurant_review_clean <- restaurant_review %>% 
  filter(review_id %in% kept_review_ids)

# 对清理后的原始数据进行保存
write.csv(restaurant_review_clean,file = "restaurant_review_clean.csv", row.names = FALSE)
restaurant_review_clean <- read.csv("restaurant_review_clean.csv",header = TRUE)

# 看常见词
top_words <- textstat_frequency(review_dfm, n = 100)
top_words_stem <- textstat_frequency(review_dfm_stem, n = 100)

# visualization of high frequency words
ggplot(head(top_words,20), aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(fill = "#800020") +
  coord_flip() +  
  labs(title = "Top 20 Most Frequent Features",
       x = "Feature",
       y = "Frequency") +
  theme_minimal()

ggplot(head(top_words_stem,20), aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(fill = "#800020") +
  coord_flip() +  
  labs(title = "Top 20 Most Frequent Features (stem)",
       x = "Feature",
       y = "Frequency") +
  theme_minimal()

#######################################################
# 3️⃣ NLP step 2-- LDA 主题分析 使用stem过的 --------------
#######################################################

# 先取样本
set.seed(1234)
sampled_ids <- sample(kept_review_ids, size = 5000)
review_sample <- restaurant_review_clean %>% filter(review_id %in% sampled_ids)
review_dfm_stem_sample <- review_dfm_stem[sampled_ids, ]

# trim dfm
review_dfm_stem_sample_trim <- dfm_trim(review_dfm_stem_sample, min_termfreq = 5)

# convert dfm to dtm
dtm_stem_sample <- convert(review_dfm_stem_sample_trim, to = "topicmodels")

# 保存dtm,方便后续直接使用
saveRDS(dtm_stem_sample, file = "dtm_stem_sample.rds")
dtm_stem_sample <- readRDS("dtm_stem_sample.rds")

# 决定主题数量 (这里有个问题,ldatuning这个包需要R4.3,升级完之后用不了ldatuning)
result <- FindTopicsNumber(
  dtm_stem_sample,
  topics = seq(2, 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 1,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# perform topic modeling 
lda_model <- LDA(dtm_stem_sample, k = 17, method = "Gibbs",
                 control = list(seed = 1234))


saveRDS(lda_model, file = "lda_model.rds")
lda_model <- readRDS("lda_model.rds")

# 看主题的出现次数
# Extract the gamma matrix 
gamma_values <- tidy(lda_model, matrix = "gamma")

# Create grouped gamma tibble
grouped_gammas <- gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
# Count (tally) by topic
grouped_gammas %>% 
  tally(topic, sort=TRUE)

# 看每个topic的高频词
# Extract top words per topic
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 20) %>%  # 每个主题取前20个词
  ungroup()

# Print the top words per topic
print(top_terms)

#  Visualize the Results
# Barplot of top words per topic
top_terms %>%
  ggplot(aes(x = reorder_within(term, beta, topic),
             y = beta,
             fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +  
  labs(title = "Top Words in Each Topic",
       x = "Terms", y = "Beta Score") +
  theme_minimal()


# 给topic命名
topic_labels <- c(
  "Tp1_Recommendation_and_Service",          # make, friend, recommend, always
  "Tp2_Dinner_and_Steak",                    # restaur, dinner, meat, steak
  "Tp3_Return_and_Impression",              # tri, place, back, come
  "Tp4_Price_and_Quality",                   # price, fresh, portion, quality
  "Tp5_Menu_and_Restaurant",                 # menu, restaur, option, item
  "Tp6_Breakfast_and_Bacon",                 # breakfast, bacon, egg, sausage
  "Tp7_Timing_and_Arrival",                  # time, now, early, day
  "Tp8_Order_and_Waffle",                    # order, get, waffle, wait
  "Tp9_Positive_Emotion",                    # good, love, amazing
  "Tp10_Burgers_and_Fries",                  # chicken, fri, burger, sandwich
  "Tp11_Soups_and_Asian_Food",              # dish, soup, noodle, asian
  "Tp12_Great_Atmosphere_and_Staff",         # great, friend, recommend, atmosphere
  "Tp13_Pizza_and_Cheesesteak",              # pizza, salad, cheesesteak
  "Tp14_Mexican_Flavors",                    # flavor, taco, mexican
  "Tp15_Bar_and_Drinks",                     # bar, dinner, drink
  "Tp16_Local_and_place",                    # local, food, clear
  "Tp17_Subjective_Feelings"                # like, feel, though
)

# 相对来说,一下话题与餐厅本身比较无关
# Tp8 内容偏泛化褒奖，如 love, best, amazing；缺乏细节
# Tp13 like, want, feel 等词主观情感强、不特指餐厅服务/菜品
# Tp15 like, go, eat, want, feel 等泛泛而谈，不具体指涉菜单/服务等
# 以上tp联合其他话题都能有用,但是如果只有以上显形是将认为是irrelevant

# 提取主题分布
theta_all <- as.data.frame(posterior(lda_model)$topics)
colnames(theta_all) <- topic_labels

# 添加 review_id 并合并
sample_topic <- cbind(review_sample,theta_all)

write.csv(sample_topic,file = "sample_topic.csv", row.names = FALSE)
sample_topic <- read.csv("sample_topic.csv",header = TRUE)

# 通过话题百分比和加权来计算相关性 ----
topic_weights <- c(
  1.0,  # Tp1：推荐 + 服务
  1.0,  # Tp2：晚餐主菜
  0.8,  # Tp3：顾客回访
  1.0,  # Tp4：食物价格和质量
  0.8,  # Tp5：菜单内容
  1.0,  # Tp6：早餐主食
  0.6,  # Tp7：时间/到达时间
  0.8,  # Tp8：点单流程
  0.6,  # Tp9：积极情感（不具体）
  1.0,  # Tp10：汉堡薯条
  1.0,  # Tp11：亚洲菜/汤类
  1.0,  # Tp12：氛围 + 服务
  1.0,  # Tp13：披萨/奶酪三明治
  1.0,  # Tp14：墨西哥口味
  0.8,  # Tp15：酒吧饮品
  0.6,  # Tp16：本地/地点
  0.6   # Tp17：主观感受偏泛
)

## 找出 LDA 分布列名
topic_cols <- grep("^Tp", names(sample_topic), value = TRUE)

# 计算 relevance score
sample_topic$relevant <- as.numeric(as.matrix(sample_topic[, topic_cols]) %*% topic_weights)

ggplot(sample_topic,aes(relevant))+
  geom_histogram(fill= "#800020",color="white")+
  labs(title = "Distribution of Relevance of Each Review",
       y = "Frequency",
       x = "relevant")

#以topic 1/2为例子观测分布频率
ggplot(sample_topic,aes(Tp1_Recommendation_and_Service))+
  geom_histogram(fill= "#800020",color="white")
ggplot(sample_topic,aes(Tp2_Dinner_and_Steak))+
  geom_histogram(fill= "#800020",color="white")

# 通过阈值设置将TP变成boolean 
summary(sample_topic)

# 将所有主题二值化（>= 0.08）
for (tp in topic_labels) {
  sample_topic[[tp]] <- if_else(sample_topic[[tp]] >= 0.08, 1, 0)
}

# comprehensive = 主题百分比超过阈值的主题数量之和
sample_topic <- sample_topic %>%
  mutate(comprehensive = rowSums(across(all_of(topic_labels))))

ggplot(sample_topic,aes(comprehensive))+
  geom_bar(fill= "#800020",color="white")+
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of comprehensive of Each Review",
       y = "Frequency",
       x = "Number of topics")

sample_topic <- sample_topic %>% select(-Unnamed..0)

sample_step1_done <- sample_topic 
colSums(is.na(sample_step1_done))

# review的clarity
spelling_errors <- hunspell(sample_step1_done$text)
error_count <- sapply(spelling_errors, length)
word_count <- sapply(strsplit(sample_step1_done$text, "\\s+"), length)
spelling_error_rate <- error_count / word_count
sample_step1_done$clarity <- 1-spelling_error_rate

write.csv(sample_step1_done,file = "sample_step1_done.csv", row.names = FALSE)
sample_step1_done <- read.csv("sample_step1_done.csv",header = TRUE)

#######################################################
# 4️⃣ NLP step 3-- sentiment 分析
#######################################################

## 已经训练好的深度学习(BERT)的NLP模型 ------------

# 安装 reticulate 包
#install.packages("reticulate")
library(reticulate)

#reticulate::install_miniconda()
py_install(c("transformers", "torch"))

# 2. 加载 Python 模型
transformers <- import("transformers")
torch <- import("torch")

tokenizer <- transformers$AutoTokenizer$from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")
model <- transformers$AutoModelForSequenceClassification$from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")

reticulate::py_run_string("
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import torch
import torch.nn.functional as F

tokenizer = AutoTokenizer.from_pretrained('nlptown/bert-base-multilingual-uncased-sentiment')
model = AutoModelForSequenceClassification.from_pretrained('nlptown/bert-base-multilingual-uncased-sentiment')

def predict_single_sentiment(text):
    inputs = tokenizer(text, return_tensors='pt', truncation=True)
    with torch.no_grad():
        outputs = model(**inputs)
        probs = F.softmax(outputs.logits, dim=1)
        probs = probs.numpy().flatten()
        label = probs.argmax() + 1  # labels are 1-indexed
    return label, probs.tolist()
")

# 设置进行Bert情感分析的函数
predict_sentiment <- function(texts) {
  results <- lapply(texts, function(txt) {
    out <- reticulate::py$predict_single_sentiment(txt)
    list(label = out[[1]], probs = unlist(out[[2]]))
  })
  return(results)
}

# 使用bert进行情感分析
texts <- sample_step1_done$text
sentiment_results <- predict_sentiment(texts)

saveRDS(sentiment_results, file = "sentiment_results.rds")
sentiment_results <- readRDS("sentiment_results.rds")

sample_step1_done <- sample_step1_done %>%
  mutate(
    bert_score = sapply(sentiment_results, function(x) x$label),
    prob_1star = sapply(sentiment_results, function(x) x$probs[1]),
    prob_2star = sapply(sentiment_results, function(x) x$probs[2]),
    prob_3star = sapply(sentiment_results, function(x) x$probs[3]),
    prob_4star = sapply(sentiment_results, function(x) x$probs[4]),
    prob_5star = sapply(sentiment_results, function(x) x$probs[5])
  )
# 使用bert score来搞objective
sample_step1_done$bert_objective <- 1 - (abs(sample_step1_done$bert_score - 3) / 2)
# 因为在CFA分析的地方需要linear一点的变量,因此通过jitter使变量的值变多
set.seed(1234) 
sample_step1_done$bert_objective <- jitter(sample_step1_done$bert_objective, amount = 0.05)

sample_step1_done <- sample_step1_done %>%
  select(-prob_1star, -prob_2star, -prob_3star, -prob_4star, -prob_5star)

sample_step2_done <- sample_step1_done

ggplot(sample_step2_done,aes(bert_score))+
  geom_bar(fill= "#800020",color="white")+
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of bert_score",
       y = "Frequency",
       x = "bert_score")

# 使用bert|afinn来区分valence positive(1)/neutral(0)/negative(-1)
sample_step2_done <- sample_step2_done %>% 
  mutate(bert_valence = case_when(bert_score > 3 ~  1,
                                  bert_score < 3 ~ -1,
                                  bert_score ==3 ~  0,
                                  TRUE ~ NA)) 

summary(sample_step2_done[, c("comprehensive", "clarity", "bert_score", "bert_objective", "bert_valence")])

write.csv(sample_step2_done,file = "sample_step2_done.csv", row.names = FALSE)
sample_step2_done <- read.csv("sample_step2_done.csv",header = TRUE)

#######################################################
# 5️⃣ Descriptive Analysis --------------
#######################################################
sample_step2_done <- sample_step2_done %>% 
  select(-address,-postal_code,-latitude,-longitude,-is_open,
         -attributes,-hours)

colSums(is.na(sample_step2_done))
summary(sample_step2_done[, c(
  "useful", "new_reviews_3m", 
  "relevant", "comprehensive", "clarity", "bert_score", 
  "bert_objective", "bert_valence", "membership_length", "user_avg_useful"
)])

summary(sample_step2_done %>% select(user_id,user_avg_useful,fans,average_stars,elite_years,friend_count) %>%
          distinct(user_id, .keep_all = TRUE) )

# visualization
df_long <- sample_step2_done %>%
  select(all_of(c("useful", "new_reviews_3m", 
                  "relevant", "comprehensive", "clarity", "bert_score", 
                  "bert_objective", "bert_valence", "membership_length",
                  "user_avg_useful","average_stars","elite_years"))) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")

summary_counts <- sample_step2_done %>%
  summarise(
    total_reviews = n(),
    unique_businesses = n_distinct(business_id),
    unique_users = n_distinct(user_id),
    unique_states = n_distinct(state)
  )
summary_counts

# 绘制多变量的独立 boxplot（每个变量一个小图）
ggplot(df_long, aes(x = "", y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # 每个小图独立坐标轴
  theme_minimal() +
  labs(title = "Boxplots of Each Variable (with Independent Y Scales)", x = "", y = "") +
  theme(strip.text = element_text(size = 10))

# correlation plot
# 选择指定列
selected_cols <- sample_step2_done %>%
  select(as.numeric(all_of(c("useful", "new_reviews_3m", 
                             "relevant", "comprehensive", "clarity", "bert_objective", "word_count", 
                             "bert_valence", "membership_length","elite_years", "user_avg_useful")))) 

# 计算每个变量的均值和标准差
descriptive_stats <- sample_step2_done %>%
  select(all_of(c("useful", "new_reviews_3m", 
                  "relevant", "comprehensive", "clarity", "bert_score", 
                  "bert_objective", "bert_valence", "membership_length",
                  "user_avg_useful", "average_stars", "elite_years"))) %>%
  summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE),
                                      sd = ~sd(., na.rm = TRUE))))
descriptive_stats

# 将逻辑型变量转换为数值型
selected_cols <- selected_cols %>%
  mutate(across(where(is.logical), as.numeric))

# 计算相关矩阵（自动排除 NA）
cor_matrix <- cor(selected_cols, use = "pairwise.complete.obs")

# 绘制相关图
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.6, col = colorRampPalette(c("steelblue", "white", "tomato"))(200))

sample_step3_done <- sample_step2_done

write.csv(sample_step3_done, "sample_step3_done.csv", row.names = FALSE)
sample_step3_done <- read.csv("sample_step3_done.csv",header = TRUE)

#######################################################
# 6️⃣ SEM Modeling --------------
#######################################################
#“Instead of using automated cross-validation, this study applied five-fold random subsetting to 
#manually assess model stability across samples. The approach allowed direct inspection of CFA loadings 
#and SEM paths, offering transparent robustness checks.”
# =============================================
# ✅ 1. 数据准备与中心化变量 + 交互项构建
# =============================================

# 创建useful_dummy (避免由于useful的零膨胀和skew特征影响sem的估计)
sample_step3_done <- sample_step3_done %>% mutate(
  useful_dummy = if_else(useful==0,0,1))

# 交互项创建中心化变量
sample_step3_done <- sample_step3_done %>%
  mutate(
    valence_c = scale(bert_valence, center = TRUE, scale = FALSE),
    int_use_val = useful_dummy * valence_c
  )

# 观测变量进行标准化
sample_step3_done <- sample_step3_done %>%
  mutate(
    clarity = as.numeric(scale(clarity, center = TRUE, scale = TRUE)),
    comprehensive = as.numeric(scale(comprehensive, center = TRUE, scale = TRUE)),
    user_avg_useful = as.numeric(scale(user_avg_useful, center = TRUE, scale = TRUE)),
    elite_years = as.numeric(scale(elite_years, center = TRUE, scale = TRUE)),
    membership_length = as.numeric(scale(membership_length, center = TRUE, scale = TRUE)),
    bert_objective = as.numeric(scale(bert_objective, center = TRUE, scale = TRUE)),
    stars = as.numeric(scale(stars, center = TRUE, scale = TRUE)),
    review_count = as.numeric(scale(review_count, center = TRUE, scale = TRUE)),
    relevant = as.numeric(scale(relevant, center = TRUE, scale = TRUE))
  )

# 对因变量进行log处理
sample_step3_done$new_reviews_3m_ln <- log1p(sample_step3_done$new_reviews_3m)

# 对文字变量进行factor处理
sample_step3_done$name <- as.factor(sample_step3_done$name)
sample_step3_done$state <- as.factor(sample_step3_done$state)
sample_step3_done$year_quarter <- as.factor(sample_step3_done$year_quarter)
summary(sample_step3_done)

# ===============================
# Step 1: 拆分数据
# ===============================
set.seed(123)
n <- nrow(sample_step3_done)
split_indices <- split(1:n, cut(1:n, breaks = 5, labels = FALSE))
data_list <- lapply(split_indices, function(idx) sample_step3_done[idx, ])

# ===============================
# Step 2: 循环CFA分析
# ===============================
# 创建空数据库
cfa_results <- list()
cfa_fitmeasures <- data.frame()
cfa_results_plot <- list()

for (i in 1:5) {
  data_i <- data_list[[i]]
  
  cfa_model <- '
    Argument_Quality =~ clarity  +  word_count + relevant + bert_objective + comprehensive
    Source_Credibility =~ user_avg_useful + elite_years + membership_length
  '
  
  fit_cfa <- cfa(cfa_model, data = data_i, std.lv = TRUE)
  cfa_results[[i]] <- parameterEstimates(fit_cfa, standardized = TRUE)
  
  cfa_results_plot[[i]] <- fit_cfa
  
  fitm <- fitMeasures(fit_cfa, c("cfi", "tli", "rmsea", "srmr"))
  cfa_fitmeasures <- rbind(cfa_fitmeasures, 
                           data.frame(set = i,
                                      cfi = fitm["cfi"],
                                      tli = fitm["tli"],
                                      rmsea = fitm["rmsea"],
                                      srmr = fitm["srmr"]))
}

# 参数结果
cfa_results[[1]]
cfa_results[[2]]
cfa_results[[3]]
cfa_results[[4]]
cfa_results[[5]]

# 拟合指标
cfa_fitmeasures 

# 可视化
for (i in seq_along(cfa_results_plot)) {
  cat("\n📊 正在显示第", i, "个数据子集的 SEM 路径图...\n")
  
  semPaths(
    cfa_results_plot[[i]],
    what = "std",              # 使用标准化路径系数
    layout = "tree",           # 树状结构
    style = "lisrel",          # LISREL 风格
    nCharNodes = 0,            # 不截断变量名
    residuals = FALSE,         # 不显示残差箭头
    title = FALSE,             # 不显示标题
    #    fade = FALSE
    #    ,              # 取消透明度淡化
    sig = 0.05,                  # 显著性标星号阈值
    edge.label.sig = TRUE        # 添加显著性
  )
  
  # 可选：暂停查看（按回车继续）
  readline(prompt = "按回车查看下一个图...")
}

# ===============================
# Step 4: 循环SEM_1分析
# ===============================

# 创建空数据框
sem_results <- list()
sem_results_plot <- list()
sem_fitmeasures <- data.frame()

for (i in 1:5) {
  data_i <- data_list[[i]]
  
  sem_model <- '
    Argument_Quality =~ clarity  +  word_count + relevant + bert_objective + comprehensive
    Source_Credibility =~ user_avg_useful + elite_years + membership_length
    useful_dummy ~ Argument_Quality + Source_Credibility
  '
  #due to the non-normality of useful, I choosed MLR a Robust Maximum Likelihood method
  fit_sem <- sem(sem_model, data = data_i, std.lv = TRUE, estimator = "MLR") 
  
  sem_results[[i]] <- parameterEstimates(fit_sem, standardized = TRUE)
  # ✔ 保存完整模型对象（不是summary或参数）
  sem_results_plot[[i]] <- fit_sem
  
  # ✔ 同时存fit指标（可选）
  fitm <- fitMeasures(fit_sem, c("cfi", "tli", "rmsea", "srmr"))
  sem_fitmeasures <- rbind(sem_fitmeasures,
                           data.frame(set = i,
                                      cfi = fitm["cfi"],
                                      tli = fitm["tli"],
                                      rmsea = fitm["rmsea"],
                                      srmr = fitm["srmr"]))
}

# 拟合指标
sem_fitmeasures 

# 参数结果
sem_results[[1]]
sem_results[[2]]
sem_results[[3]]
sem_results[[4]]
sem_results[[5]]

# 可视化
for (i in seq_along(sem_results_plot)) {
  cat("\n📊 正在显示第", i, "个数据子集的 SEM 路径图...\n")
  
  semPaths(
    sem_results_plot[[i]],
    what = "std",              # 使用标准化路径系数
    layout = "tree",           # 树状结构
    style = "lisrel",          # LISREL 风格
    nCharNodes = 0,            # 不截断变量名
    residuals = FALSE,         # 不显示残差箭头
    title = FALSE,             # 不显示标题
    #    fade = FALSE
    #    ,              # 取消透明度淡化
    sig = 0.05,                  # 显著性标星号阈值
    edge.label.sig = TRUE        # 添加显著性
  )
  
  # 可选：暂停查看（按回车继续）
  readline(prompt = "按回车查看下一个图...")
}

#################################
# Step 5: 将完整模型放入 SEM_2
#################################
# 初始化列表
sem_full_results <- list()
sem_full_results_plot <- list()
sem_full_fitmeasures <- data.frame()


for (i in 1:5) {
  data_i <- data_list[[i]]
  
  
  # SEM模型：包含 useful → new_reviews_3m 的结构路径
  sem_model_full <- '
    # Measurement model
    Argument_Quality =~ clarity + word_count + relevant + bert_objective + comprehensive
    Source_Credibility =~ user_avg_useful + elite_years + membership_length

    # Structural model
    useful_dummy ~ Argument_Quality + Source_Credibility
    new_reviews_3m_ln ~ useful_dummy + valence_c + int_use_val + review_count
  '
  
  # ⚠ 使用 MLR 做鲁棒估计，因包含计数型数据
  fit_sem_full <- sem(sem_model_full, data = data_i, std.lv = TRUE, estimator = "MLR")
  
  # 保存结果
  sem_full_results[[i]] <- parameterEstimates(fit_sem_full, standardized = TRUE)
  sem_full_results_plot[[i]] <- fit_sem_full
  
  # 提取拟合指标
  fitm <- fitMeasures(fit_sem_full, c("cfi", "tli", "rmsea", "srmr"))
  sem_full_fitmeasures <- rbind(sem_full_fitmeasures,
                                data.frame(set = i,
                                           cfi = fitm["cfi"],
                                           tli = fitm["tli"],
                                           rmsea = fitm["rmsea"],
                                           srmr = fitm["srmr"]))
}

# 拟合指标
sem_full_fitmeasures

# 参数结果
sem_full_results[[1]]
sem_full_results[[2]]
sem_full_results[[3]]
sem_full_results[[4]]
sem_full_results[[5]]


for (i in seq_along(sem_full_results_plot)) {
  cat("\n📊 正在显示第", i, "个子集的 SEM 全路径图...\n")
  
  semPaths(
    sem_full_results_plot[[i]],
    what = "std",              # 使用标准化路径系数
    layout = "tree",           # 树状结构
    style = "lisrel",          # LISREL 风格
    nCharNodes = 0,            # 不截断变量名
    residuals = FALSE,         # 不显示残差箭头
    title = FALSE,             # 不显示标题
    #    fade = FALSE
    #    ,              # 取消透明度淡化
    sig = 0.05,                  # 显著性标星号阈值
    edge.label.sig = TRUE        # 添加显著性
  )
  
  readline(prompt = "按回车查看下一个图...")
}


summary(sample_step3_done)
sd(sample_step3_done$word_count)
mean(sample_step3_done$word_count)

#######################################################
# 7️⃣ 负二项回归(使用完整数据集) --------------
#######################################################


## 已经训练好的深度学习(BERT)的NLP模型 ------------
# 安装 reticulate 包
#install.packages("reticulate")
library(reticulate)

#reticulate::install_miniconda()

py_install(c("transformers", "torch"))

# 2. 加载 Python 模型
transformers <- import("transformers")
torch <- import("torch")

tokenizer <- transformers$AutoTokenizer$from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")
model <- transformers$AutoModelForSequenceClassification$from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")

reticulate::py_run_string("
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import torch
import torch.nn.functional as F

tokenizer = AutoTokenizer.from_pretrained('nlptown/bert-base-multilingual-uncased-sentiment')
model = AutoModelForSequenceClassification.from_pretrained('nlptown/bert-base-multilingual-uncased-sentiment')

def predict_single_sentiment(text):
    inputs = tokenizer(text, return_tensors='pt', truncation=True)
    with torch.no_grad():
        outputs = model(**inputs)
        probs = F.softmax(outputs.logits, dim=1)
        probs = probs.numpy().flatten()
        label = probs.argmax() + 1  # labels are 1-indexed
    return label, probs.tolist()
")


predict_sentiment <- function(texts) {
  results <- lapply(texts, function(txt) {
    out <- reticulate::py$predict_single_sentiment(txt)
    list(label = out[[1]], probs = unlist(out[[2]]))
  })
  return(results)
}

texts <- restaurant_review_clean$text

# 使用bert来分析sentiment
sentiment_results_full <- predict_sentiment(texts)
restaurant_review_full_with_bert <- restaurant_review %>%
  mutate(
    bert_score = sapply(sentiment_results_full, function(x) x$label),
    prob_1star = sapply(sentiment_results_full, function(x) x$probs[1]),
    prob_2star = sapply(sentiment_results_full, function(x) x$probs[2]),
    prob_3star = sapply(sentiment_results_full, function(x) x$probs[3]),
    prob_4star = sapply(sentiment_results_full, function(x) x$probs[4]),
    prob_5star = sapply(sentiment_results_full, function(x) x$probs[5])
  ) 

# 使用bert score来构建objectiveness
restaurant_review_full_with_bert$bert_objective <- 1 - (abs(restaurant_review_full_with_bert$bert_score - 3) / 2)
set.seed(1234)
restaurant_review_full_with_bert$bert_objective <- jitter(restaurant_review_full_with_bert$bert_objective, amount = 0.05)

restaurant_review_full_with_bert <- restaurant_review_full_with_bert %>%
  select(-prob_1star, -prob_2star, -prob_3star, -prob_4star, -prob_5star)

ggplot(restaurant_review_full_with_bert,aes(bert_score))+
  geom_bar(fill= "#800020",color="white")+
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of bert_score",
       y = "Frequency",
       x = "bert_score")

# 使用bert区分valence positive(1)/neutral(0)/negative(-1)
restaurant_review_full_with_bert <- restaurant_review_full_with_bert %>% 
  mutate(bert_valence = case_when(bert_score > 3 ~  1,
                                  bert_score < 3 ~ -1,
                                  bert_score ==3 ~  0,
                                  TRUE ~ NA))

summary(restaurant_review_full_with_bert[,c(30:42)])

##########################
# Descriptive Analysis --------------
##########################

summary_counts <- restaurant_review_full_with_bert %>%
  summarise(
    total_reviews = n(),
    unique_businesses = n_distinct(business_id),
    unique_users = n_distinct(user_id),
    unique_states = n_distinct(state)
  )
summary_counts

# 计算每个变量的均值和标准差
descriptive_stats <- restaurant_review_full_with_bert %>%
  select(all_of(c("useful", "new_reviews_3m", 
                   "bert_valence", "membership_length",
                  "user_avg_useful", "average_stars", "elite_years"))) %>%
  summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE),
                                      sd = ~sd(., na.rm = TRUE))))
descriptive_stats

# 选择指定列
selected_cols <- restaurant_review_full_with_bert %>%
  select(as.numeric(all_of(c("useful", "new_reviews_3m", 
                             "relevant", "comprehensive", "clarity", "bert_objective", "word_count", 
                             "bert_valence", "membership_length","elite_years", "user_avg_useful")))) 

# 将逻辑型变量转换为数值型
selected_cols <- selected_cols %>%
  mutate(across(where(is.logical), as.numeric))

# 计算相关矩阵（自动排除 NA）
cor_matrix <- cor(selected_cols, use = "pairwise.complete.obs")

# 绘制相关图
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.6, col = colorRampPalette(c("steelblue", "white", "tomato"))(200))

# 看看useful的distribution
ggplot(restaurant_review_full_with_bert,aes(useful))+
  geom_bar(fill= "#800020",color="white")+
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of useful",
       y = "Frequency",
       x = "useful")

# 看看EB的distribution
ggplot(restaurant_review_full_with_bert,aes(new_reviews_3m))+
  geom_bar(fill= "#800020",color="white")+
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.2)+
  labs(title = "Distribution of new reviews 3 months after each review",
       y = "Frequency",
       x = "useful")
summary(restaurant_review_full_with_bert$new_reviews_3m)

# 看看word count的distribution
ggplot(restaurant_review_full_with_bert,aes(word_count))+
  geom_histogram(fill= "#800020",color="white")+
  labs(title = "Distribution of word count",
       y = "Frequency",
       x = "useful")
summary(restaurant_review_full_with_bert$new_reviews_3m)

# 看看valence的distribution
ggplot(restaurant_review_full_with_bert, aes(x = bert_valence)) +
  geom_bar(fill = "#800020", color = "white", width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4) +
  labs(title = "Distribution of Valence",
       x = "Valence Category",
       y = "Frequency") +
  theme_minimal()

# 统计每个州的评论数，并按数量排序
state_review_counts <- restaurant_review_full_with_bert %>%
  group_by(state) %>%
  summarise(review_count = n()) %>%
  arrange(desc(review_count))

# 绘制图表
ggplot(state_review_counts, aes(x = reorder(state, -review_count), y = review_count)) +
  geom_bar(stat = "identity", fill = "#800020") +
  geom_text(aes(label = review_count), 
            vjust = -0.5, size = 3.2) +
  labs(title = "Number of Reviews per State",
       x = "State",
       y = "Total Reviews") +
  theme_minimal()

# 每个州的平均RU
ggplot(state_means, aes(x = reorder(state, -avg_business_stars), y = avg_business_stars)) +
  geom_bar(stat = "identity", fill = "#800020") +
  geom_text(aes(label = round(avg_business_stars, 2)), vjust = -0.5, size = 3.2) +
  labs(title = "Average Business Stars per State",
       x = "State",
       y = "Average Stars") +
  theme_minimal()

# 每个州的平均EB
ggplot(state_means, aes(x = reorder(state, -avg_new_reviews_3m), y = avg_new_reviews_3m)) +
  geom_bar(stat = "identity", fill = "#800020") +
  geom_text(aes(label = round(avg_new_reviews_3m, 1)), vjust = -0.5, size = 3.2) +
  labs(title = "Average New Reviews in 3 Months per State",
       x = "State",
       y = "Avg. New Reviews") +
  theme_minimal()

# 每个州的平均店铺有用投票
ggplot(state_means, aes(x = reorder(state, -avg_useful), y = avg_useful)) +
  geom_bar(stat = "identity", fill = "#800020") +
  geom_text(aes(label = round(avg_useful, 1)), vjust = -0.5, size = 3.2) +
  labs(title = "Average Review Usefulness per State",
       x = "State",
       y = "Avg. Useful Score") +
  theme_minimal()


# 中心化RU和valence,并创建交互项
restaurant_review_full_with_bert <- restaurant_review_full_with_bert %>%
  mutate(
    useful_c = scale(useful, center = TRUE, scale = FALSE),
    valence_c = scale(bert_valence, center = TRUE, scale = FALSE),
    int_use_val = useful_c * valence_c
  )

# 观测变量进行标准化
restaurant_review_full_with_bert <- restaurant_review_full_with_bert %>%
  mutate(
    review_count = as.numeric(scale(review_count, center = TRUE, scale = TRUE)),
  )

# 将文字转换成因子
restaurant_review_full_with_bert$state <- as.factor(restaurant_review_full_with_bert$state)
restaurant_review_full_with_bert$year_quarter <- as.factor(restaurant_review_full_with_bert$year_quarter)
str(restaurant_review_full_with_bert)
summary(restaurant_review_full_with_bert)

# 一次性 dummy 化 state 和 year_quarter（共 17 + 20 个 dummy）
#install.packages("fastDummies")
restaurant_review_full_with_bert <- fastDummies::dummy_cols(
  restaurant_review_full_with_bert,
  select_columns = c("state", "year_quarter"),
  remove_selected_columns = TRUE,  # 去掉原始列
  remove_first_dummy = TRUE        # 避免多重共线性（留出 reference 类别）
)
# 把所有列名中的 "-" 替换成 "_"（只替换列名，不改内容）
names(restaurant_review_full_with_bert) <- gsub("-", "_", names(restaurant_review_full_with_bert))
colSums(restaurant_review_full_with_bert[,37:70])

################
# pre-test for NBR -----------
################
# 均值 vs 方差检验（判断是否过度离散）
set.seed(1234)  # 可复现的划分
n_total <- nrow(restaurant_review_full_with_bert)
train_indices <- sample(1:n_total, size = 0.8 * n_total)

train_df <- restaurant_review_full_with_bert[train_indices, ]
test_df  <- restaurant_review_full_with_bert[-train_indices, ]

mean(train_df$new_reviews_3m)
var(train_df$new_reviews_3m)
dispersion_ratio <- var(train_df$new_reviews_3m) / mean(train_df$new_reviews_3m)
# 如果 Variance >> Mean（比如 ratio > 1.5~2），说明有明显过度离散, Dispersion_Ratio = 45 所以通过

# 0的比例分析（判断是否零膨胀）
zero_rate <- mean(train_df$new_reviews_3m == 0)
cat("Proportion of 0s:", round(zero_rate * 100, 2), "%\n")
# 0只占了5.67%,不需要0膨胀

# 对比一下泊松和负二

pois_model <- glm(new_reviews_3m ~ useful_c + valence_c + int_use_val,
                  family = poisson, data = train_df)
nb_model <- glm.nb(new_reviews_3m ~ useful_c + valence_c + int_use_val,
                   data = train_df)
AIC(pois_model, nb_model)

#对比一下负二和零膨胀负二
zinb <- zeroinfl(
  new_reviews_3m ~ useful_c + valence_c + int_use_val | 1,
  data = train_df,
  dist = "negbin"
)
vuong(zinb, nb_model)

#Raw 检验无显著差异 → 模型之间无法区分；
#AIC/BIC 校正后都显著 < 0：
#z 值为负 → NB 更好；
#p < 0.001 → 差异非常显著；
#用 Negative Binomial 更合适，ZINB 反而过拟合

# 构建模型
# NBR_1：主效应
nb1 <- glm.nb(new_reviews_3m ~ useful_c + valence_c, data = train_df)
summary(nb1)

# NBR_2：加交互项
nb2 <- glm.nb(new_reviews_3m ~ useful_c + valence_c + int_use_val, data = train_df)
summary(nb2)

# NBR_3：加入控制变量和dummy
nb3 <- glm.nb(new_reviews_3m ~ useful_c + valence_c + int_use_val + review_count +
                state_AZ + state_CA + state_DE + state_FL + state_ID + state_IL + state_IN +
                state_LA + state_MO +  state_NJ + state_NV + state_PA + state_TN +
                year_quarter_2017_Q2 + year_quarter_2017_Q3 + year_quarter_2017_Q4 +
                year_quarter_2018_Q1 + year_quarter_2018_Q2 + year_quarter_2018_Q3 + year_quarter_2018_Q4 +
                year_quarter_2019_Q1 + year_quarter_2019_Q2 + year_quarter_2019_Q3 + year_quarter_2019_Q4 +
                year_quarter_2020_Q1 + year_quarter_2020_Q2 + year_quarter_2020_Q3 + year_quarter_2020_Q4 +
                year_quarter_2021_Q1 + year_quarter_2021_Q2 + year_quarter_2021_Q3,
              data = train_df)
summary(nb3)

# Calculate McFadden R² + Nagelkerke R²
# 模型 1：主效应
cat("✅ Model 1 (Main Effects):\n")
r2_nb1 <- pR2(nb1)
cat("📘  McFadden's R²: ", round(r2_nb1["McFadden"], 4), "\n")
cat("📗  Nagelkerke R²:", round(r2_nb1["r2CU"], 4), "\n\n")  # r2CU 是 Nagelkerke

# 模型 2：含交互项
cat("✅ Model 2 (Interaction):\n")
r2_nb2 <- pR2(nb2)
cat("📘  McFadden's R²: ", round(r2_nb2["McFadden"], 4), "\n")
cat("📗  Nagelkerke R²:", round(r2_nb2["r2CU"], 4), "\n\n")

# 模型 3：含控制变量和 dummy
cat("✅ Model 3 (Full Model with Controls):\n")
r2_nb3 <- pR2(nb3)
cat("📘  McFadden's R²: ", round(r2_nb3["McFadden"], 4), "\n")
cat("📗  Nagelkerke R²:", round(r2_nb3["r2CU"], 4), "\n\n")

# 对测试集/训练集进行预测 ------------------------------
test_df$predicted_reviews <- predict(nb3, newdata = test_df, type = "response")
train_df$predicted_reviews <- predict(nb3, newdata = train_df, type = "response")

# 可视化预测效果
ggplot(test_df, aes(x = predicted_reviews, y = new_reviews_3m)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Predicted vs. Actual New Reviews (NB)",
       x = "Predicted New Reviews",
       y = "Actual New Reviews") +
  theme_minimal()

# MAE、RMSE、Hit Rate --------------------------
library(Metrics)
# MAE, RMSE, Hit Rate for train
mae_train <- mae(train_df$new_reviews_3m, train_df$predicted_reviews)
rmse_train <- rmse(train_df$new_reviews_3m, train_df$predicted_reviews)
hit_train <- mean(ifelse(train_df$new_reviews_3m > median(train_df$new_reviews_3m), 1, 0) ==
                    ifelse(train_df$predicted_reviews > median(train_df$predicted_reviews), 1, 0))

# MAE, RMSE, Hit Rate for test
mae_test <- mae(test_df$new_reviews_3m, test_df$predicted_reviews)
rmse_test <- rmse(test_df$new_reviews_3m, test_df$predicted_reviews)
hit_test <- mean(ifelse(test_df$new_reviews_3m > median(test_df$new_reviews_3m), 1, 0) ==
                   ifelse(test_df$predicted_reviews > median(test_df$predicted_reviews), 1, 0))

# 输出
cat("📘 Train MAE:", round(mae_train, 2), " | Test MAE:", round(mae_test, 2), "\n")
cat("📘 Train RMSE:", round(rmse_train, 2), " | Test RMSE:", round(rmse_test, 2), "\n")
cat("📘 Train Hit Rate:", round(hit_train, 4), " | Test Hit Rate:", round(hit_test, 4), "\n")

# 模拟交互图 -----------------------------------
interaction_grid <- expand.grid(
  valence_c = seq(-1.5, 0.5, length.out = 50),
  useful_c = c(0, 40, 80, 120)
)
interaction_grid$int_use_val <- interaction_grid$useful_c * interaction_grid$valence_c
interaction_grid$review_count <- mean(train_df$review_count, na.rm = TRUE)
for (col in grep("^state_|^year_quarter_", names(train_df), value = TRUE)) {
  interaction_grid[[col]] <- 0
}
interaction_grid$predicted_reviews <- predict(nb3, newdata = interaction_grid, type = "response")
interaction_grid$useful_level <- factor(interaction_grid$useful_c)

ggplot(interaction_grid, aes(x = valence_c, y = predicted_reviews, color = useful_level)) +
  geom_line(size = 1.2) +
  labs(
    title = "Moderation Effect: Usefulness x Valence (NB)",
    x = "Valence (centered)",
    y = "Predicted New Reviews",
    color = "Usefulness"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("0" = "#a6cee3", "40" = "#1f78b4", "80" = "#33a02c", "120" = "#e31a1c"))

# 残差图（NB） ----------------------------------
resid_df <- data.frame(
  fitted = predict(nb3, type = "response"),
  resid = residuals(nb3, type = "pearson")
)

ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.2, shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  coord_cartesian(ylim = c(-2, 16)) +
  labs(
    title = "Residuals vs Fitted (NB)",
    x = "Predicted values",
    y = "Pearson residuals"
  ) +
  theme_minimal()

# 多重共线性检测 --------------------------------
vif(glm.nb(new_reviews_3m ~ useful_c + valence_c + int_use_val + review_count, data = train_df))
