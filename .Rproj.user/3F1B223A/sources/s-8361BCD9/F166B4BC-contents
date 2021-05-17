
library(dplyr)
library(stringr)
library(topicmodels)
library(dplyr)
library(tidytext)

library(ggplot2)


# Read data ---------------------------------------------------------------

# demo_2 <- read.csv('D:/CEU/singer semester/unstructured-text-analysis/Final-Project-DS3/clients_data.csv', 
#                    header = FALSE)
# 
# demo_6 <- read.csv('D:/CEU/singer semester/unstructured-text-analysis/Final-Project-DS3/clients_data_attempt_3.csv', 
#                    header = FALSE)

# df <- read.csv('D:/CEU/singer semester/unstructured-text-analysis/Final-Project-DS3/clients_data_attempt_3.csv', 
#                    header = FALSE, encoding = "UTF-8")

df_clients <- read.csv('D:/CEU/singer semester/unstructured-text-analysis/Final-Project-DS3/clients_data.csv',
                    header = FALSE, encoding = "UTF-8")



# colnames(demo_6)
names(df_clients)[1] <- "post_title"
names(df_clients)[2] <- "post_date"
names(df_clients)[3] <- "post_text"



# Clean data --------------------------------------------------------------

# df[1, ]
# 
# str_replace_all(demo_6_2[1, ], "[\r\n]" , "")
# str_replace_all(demo_6_2[1, ], "[\n]" , "")

# df2 <- df
# df$post_text <- gsub("\\r\\n", "", df$post_text)


# df2$post_text <- str_replace_all( " ", "[\n\t]" , df$post_text)

# remove line breaks
for(i in 1:nrow(df_clients)){
  df_clients[i, ] <- str_replace_all(df_clients[i, ], "[\n\t]" , "")
}

# 23
nchar(df[1, 1])

# add post number
df_post_n <- df_clients %>%
  mutate(post_number = row_number())

# The first step is using the unnest_token function in the tidytext package to put each word in a separate row. As you can see, the dimensions are now 417902 rows and 4 columns. The unnest token function also performed text cleaning by converting all upper case letters to lower case and removing all special characters and punctuation.

# 417902 rows
df_unnest <- df_post_n %>%
  unnest_tokens(word, post_text)


# Removing Numbers
# Numbers will not provide us any insight to (sentiment) analysis so we will remove them using the following code. Rows are reduced from 417902 to 412090.
df_number <- df_unnest %>% 
  filter(!grepl('[0-9]', word)) 
        
# https://bookdown.org/psonkin18/berkshire/tokenize.html#stop-word-removal
# Before removing stop words from our corpus, let’s look at the top 10 most frequently used words in all posts

tibble(df_unnest) %>%
  count(word, sort = T) 

# The top 10 list is comprised of exactly what the stop word removal process is trying to eliminate - frequently used words (i, to, the, a, and) that do not contribute to our (sentiment) analysis. We also see that there are 3024 unique words used throughout the corpus.


# Removing the stop words reduces the list from 412090 to 137958 rows
df_stop <- df_number %>% 
  anti_join(stop_words) 


tibble(df_stop) %>%
  count(word, sort = TRUE) 

# df_stop %>%
#   count(word, sort = TRUE) 



# The top 10 list of frequently used words changes drastically by removing the stop words but the lexical variety decreases slightly from 14709 words to 13034 words.

# Think about this. 1675 individual words accounted for 274132 of the total words used or almost 67%!


# 412090 total words after removing numbers, minus 137958 after stop words removed, equals 274132 / 412090 = 66.52%



df_stop %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = '#6fda44') +
  ggtitle("Most common words in posts") +
  labs(y = NULL)



# Freelancers -------------------------------------------------------------



df_freelancers <- read.csv('D:/CEU/singer semester/unstructured-text-analysis/Final-Project-DS3/freelancers_data.csv',
                                         header = FALSE, encoding = "UTF-8")
# 864 pages scraped


names(df_freelancers)[1] <- "post_title"
names(df_freelancers)[2] <- "post_date"
names(df_freelancers)[3] <- "post_text"



# remove line breaks
for(i in 1:nrow(df_freelancers)){
  df_freelancers[i, ] <- str_replace_all(df_freelancers[i, ], "[\n\t]" , "")
}

# 23
nchar(df[1, 1])

# add post number
df_f_post_n <- df_freelancers %>%
  mutate(post_number = row_number())

# The first step is using the unnest_token function in the tidytext package to put each word in a separate row. As you can see, the dimensions are now 417902 rows and 4 columns. The unnest token function also performed text cleaning by converting all upper case letters to lower case and removing all special characters and punctuation.

# 990259 rows
df_f_unnest <- df_f_post_n %>%
  unnest_tokens(word, post_text)


# Removing Numbers
# Numbers will not provide us any insight to (sentiment) analysis so we will remove them using the following code. Rows are reduced from 417902 to 412090.
df_f_number <- df_f_unnest %>% 
  filter(!grepl('[0-9]', word)) 

# https://bookdown.org/psonkin18/berkshire/tokenize.html#stop-word-removal
# Before removing stop words from our corpus, let’s look at the top 10 most frequently used words in all posts

tibble(df_f_unnest) %>%
  count(word, sort = T) 

# The top 10 list is comprised of exactly what the stop word removal process is trying to eliminate - frequently used words (i, to, the, a, and) that do not contribute to our (sentiment) analysis. We also see that there are 3024 unique words used throughout the corpus.


# Removing the stop words reduces the list from 412090 to 137958 rows
df_f_stop <- df_f_number %>% 
  anti_join(stop_words) 


tibble(df_f_stop) %>%
  count(word, sort = TRUE) 

# df_stop %>%
#   count(word, sort = TRUE) 



# The top 10 list of frequently used words changes drastically by removing the stop words but the lexical variety decreases slightly from 14709 words to 13034 words.

# Think about this. 1675 individual words accounted for 274132 of the total words used or almost 67%!


# 412090 total words after removing numbers, minus 137958 after stop words removed, equals 274132 / 412090 = 66.52%



df_f_stop %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = '#6fda44') +
  ggtitle("Most common words in posts") +
  labs(y = NULL)



ggplot(data, aes(x = i, y = beta, fill = ifelse(beta > 0, "Trump", "Obama"))) +
  geom_bar(stat = "identity", alpha = 0.75) +
  scale_x_continuous(breaks = data$i, labels = data$word, minor_breaks = NULL) +
  xlab("") +
  ylab("Coefficient Estimate") +
  coord_flip() +
  scale_fill_manual(
    guide = guide_legend(title = "Word typically used by:"),
    values = c("#446093", "#bc3939")
  ) +
  theme_bw() +
  theme(legend.position = "top")


