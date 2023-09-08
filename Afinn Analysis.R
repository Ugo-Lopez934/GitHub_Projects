
# Owner of the Script Hugo Lopez



# # Create vector of packages
my_packages <- c("dplyr", "mice", "wordcloud", "RColorBrewer", 
                 "webr", "ggrepel", "plotly", "plotrix","tidyverse", 
                 "ggplot2", "SnowballC","textdata", "textshaping","rAmCharts4", 
                 "janeaustenr", "ggthemes", "tidytext")    
#  # Load multiple packages
lapply(my_packages, require, character.only = TRUE) 

#   Set The Working Directory 
setwd("/Users/hugolopez/Desktop/Data Machine Learnign")


# I will begin the text analysis but we crate a data frame called  gb then I will remove some columns like 
## Usefulness, Cool and Funny 

g<- read_csv("GB_LandScaping_Yelp_Review.csv")

# Here removed all columns exvcept Username, Date, Location, Comment 
gb <-g [,c ("Username", "Date", "Location", "Comment", "Rating")]
# Review the top observations 
head(gb)

# Let us first install and load Julia Silge and David Robinson tidytext package. 
# Next The function I will use and  “abuse” is unnest_tokens().
library("tidytext", "janeaustenr")

# Let us now use the Function unnset_tokens()
# Create a new data frame called  "gb1". Now gb1 is only used to see weather the 
## function will execute the line of code. The unnset_tokens function creates a column
# With every sing word that was said by a customer

gb1<-gb %>% unnest_tokens("Word", "Comment")
# review the new column 
head(gb1)

# We also want to prevent the analysis in showing 6t and 6t's as two separate words
gb1 <- gb %>% unnest_tokens("Word", "Comment") %>% 
  mutate(Word = str_replace(Word, "'s", ""))

# We want to display graphically a word frequency plot
# We will create a function that will store all the operations which will repeat several times
word_frequency <- function(x, top = 10) {
  x %>% 
    count(Word, Sort = T) %>% 
    mutate(Word = factor(Word, levels = rev(unique(Word)))) %>% 
    top_n(top) %>% 
    ungroup() %>% 
    ggplot(mapping = aes(x =Word, y = n)) + 
    geom_col(show.legend = F) +
    coord_flip() + 
    labs(x = NULL)
}
# This shows 
gb1 %>% word_frequency(10)



## We are going to add an extra line of code and get rid of the conjunction words such "for", "and", "Not", "But" "or", "You"
gb2 <- gb %>% unnest_tokens("Word", "Comment") %>% 
  anti_join(stop_words, by = c("Word" = "word")) %>% 
  mutate(Word = str_replace(Word, "'s", ""))


gb2 %>% word_frequency(10)

#####  Sentimental Analysis: We Will now add the Afinn lexicon which we will create a new data frame and 
#####. add a field of attributes on words in a ordinal state like positve and negatives by numbers.
#####. Now the afinn lexicon will clean the data more

gb <-gb [,c ("Username", "Location", "Comment", "Rating")]

conclusion_afinn <- gb %>%
  unnest_tokens("Word", "Comment") %>%
  anti_join(stop_words, by=c("Word" = "word")) %>% 
  inner_join(get_sentiments("afinn"), by = c("Word" = "word"))

conclusion_afinn %>% word_frequency(10)


# Afin ploting 
## This first code will sum every positive word from all usernames who left a review  
# This third code will plot all lowest scores that correspond to the usernames who posted a review. 
# The first plot Vincent W used positive words that scored the highest while Myrose A. scored in using negative words 

conclusion_afinn %>%
  group_by(Username) %>%
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  mutate(Username = factor(Username, levels = rev(unique(Username)))) %>%
  ggplot(mapping = aes(x = Username, y = Score, fill = Username)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme_bw()  

# Top ten usernames that used positive words and scores 
custom_color <- "DodgerBlue"

conclusion_afinn %>%
  group_by(Username) %>%
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  top_n(10) %>%
  mutate(Username = factor(Username, levels = rev(unique(Username)))) %>%
  ggplot(mapping = aes(x = Username, y = Score, fill = Username)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  scale_fill_manual(values = rep(custom_color, 10)) +  
  theme_bw()

# Top 10 Usernames that used negative words and scores
conclusion_afinn %>%
  group_by(Username) %>%
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  top_n(-10) %>%
  mutate(Username = factor(Username, levels = rev(unique(Username)))) %>%
  ggplot(mapping = aes(x = Username, y = Score, fill = Username)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  scale_fill_manual(values = rainbow(length(unique(conclusion_afinn$Username)))) +
  theme_excel()
  
# The Fist code sums most frequent words said 
# The second code will sum and plot the lowest scores which means all top ten negative words.
conclusion_afinn %>%
  group_by(Word) %>% 
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
  ggplot(mapping = aes(x = Word, y = Score, fill = Word)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  scale_fill_viridis(discrete = TRUE) + 
  theme_classic()


# Top 10 negative words and scores 
conclusion_afinn %>%
  group_by(Word) %>% 
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  top_n(-10) %>%
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
  ggplot(mapping = aes(x = Word, y = Score, fill = Word)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  scale_fill_viridis(discrete = TRUE) +  
  theme_classic()

# top 10 positive words and scores
conclusion_afinn %>%
  group_by(Word) %>% 
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  top_n(10) %>%
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
  ggplot(mapping = aes(x = Word, y = Score, fill = Word)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  scale_fill_manual(values = rainbow(length(unique(conclusion_afinn$Word)))) +  
  theme_excel()  


# Will make a pie chart 
#by firs making a conditional statement at the bottom of the code  

conclusion_afinn <- conclusion_afinn [,c ( "Word", "value")]

seq <- conclusion_afinn

ifelse(seq>=0, "Positve", "Negative") 

# This code creates a data-frame and a new column called Pos.Neg which corresponds to the values that assigned and assign them string values of negative and positive
df <- mutate(seq, 'sentiment' = ifelse(seq>=0, "Positive", "Negative"))

# Now let us create count 
# There is 33 Negative and 132 positive statements with insight i can create the pie chart and waffle chart
df %>% count(sentiment)

df %>% count(value, sentiment)
# The bottom code is not perfect but it does sum all negative and positive words 
# From the Afinn lexicon. Ignore the sentiment column on the far left after executing this code
df %>% count(sentiment)

# Let us create a non-pie chart but a waffle
## But I first need to create a new data frame because the charts do not operate 
### with negative values but only absolute values
## There is 20% negative vs 80% positive values computed by the afinn lexicon not by score but by the percent of count 
## of positive and negative words said.

vals <- c(33,132)
val_names <- sprintf("%s (%s)", c("Negative","Positive"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <-val_names

waffle::waffle(vals) +
  ggthemes::scale_fill_tableau(name=NULL)


##The Pie chart will have the percent of all top three Negative and Positive Words said
## Positive: Recommend(33), Amazing(31), Happy(24).
## Negative: Hard(8), Loose(3), Died(3) 
## Other words(199)
## I installed the rAmCharts package and loaded it. 
install.packages("rAmCharts4")
library(rAmCharts4)

# I will first figure out which are the Top Three positive words with the highest 
# scores including top Top three lowest 
# The top three highest scores 34,32,24
conclusion_afinn %>%
  group_by(Word) %>% 
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  top_n(10)

# Are the top lowest scores -8, -3, -3
conclusion_afinn %>%
  group_by(Word) %>% 
  summarise(Score = sum(value)) %>%
  arrange(Score) %>%
  top_n(-10)        

# I will first remove the rows I do not need
c <- conclusion_afinn

# This code I will use to remove the words not needed so that I can sum the scores I do did need in order to create the PieChart.
c1<-c[c$Word != "amazing" & c$Word != "recommend" & c$Word != "bad" & c$Word != "died" & c$Word != "happy" & c$Word != "hard",]

# This code Will sum all scores with the exception of the words that were deleted above by this bottom code 
c1 %>%
  summarise(Score = sum(value)) # 199
# Here I created a new data frame of all words summed and said most frequently by score based on the Afinn lexicon 
datt <- data.frame(
  group = c("Recommend", "Amazing", "Happy", "Hard","Bad", "Died", "Other Words"),
  FR = c(34,32,24,8,3,3,199)
)

amPieChart(
  data = datt,
  category = "group",
  value = "FR",
  threeD = T,
  variableDepth = T,
  theme = "kelly",
  legend = "bottom",
  width = "950px",
  height = "750px",
  backgroundColor = "lightblue",
  caption = "Words Hard, Died and Bad Are All Words Most Said Negatively. Words Recommend, Amazing, and Happy Are All Words Said Positively.",
  chartTitle = "The % of Scores of All Positive and Negative Words Most Said by Clients")

# With this code is where I will check what words are said 
## Basically this grouped all top ten words that are said most frequently which is different from the summed of % potrayed above with the Pie Chart 
conclusion_afinn %>% word_frequency(10)
### Positive and Negative sentimental Analysis 
#### What I did was subset the Conclusion_afinn data frame to three variables
##. The code needs to be recreated once more for this next process 
conclusion_afinn <- gb %>%
  unnest_tokens("Word", "Comment") %>%
  anti_join(stop_words, by=c("Word" = "word")) %>% 
  inner_join(get_sentiments("afinn"), by = c("Word" = "word"))


conclusion_afinn <- conclusion_afinn [,c ("Username","Word", "value", "Rating")]

# I then created a new data frame called seq
seq <- conclusion_afinn
# What I did next is use the mutate function to create a new variable and assigned 
# it Positive and Negative rows corresponding to the values of the 2 column  
seq$sentiment <- ifelse(seq$value>0, "Positve", "Negative")
# Here I created a graph that demonstrates each clients count of Positive and Negative sentiment
seq %>%
  group_by(Username, sentiment) %>%
  count() %>%
  ungroup() %>%
  mutate(Username = reorder(Username, n)) %>%
  ggplot(mapping = aes(x = Username, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = "Negative vs Positive sentiment / Username") +
  facet_wrap(~ sentiment, ncol = 2)


# In order to figure out which customer used the negative words"hard","bad" and "died" on yelp I first 
#  need to write the code and presents those names 

#             Negative first then positive 
targetnames <- c("bad","died", "hard")
# What this code does is show me only the clients who have used these words, 
# including how many times they have said 
nat<-filter(c, Word %in% targetnames)
#  This bottom code adds the rating column and cleans the count by adding how many times the
# clients have said one of these words. The star ratting column will help identify weather 
# the negative words were wrongly interpreted ,therefore, valued bad by the afinn lexicon library.
#               Understanding "words" when used in Context is important 
nat1<- nat %>% count(Username, Rating, Word)



#             Same for the positive words 
targetnames<-c("recommend","happy","amazing")

# What this code does is show me only the clients who have used these words, 
# including how many times they have said 
datt<-filter(c, Word %in% targetnames)

# This bottom code adds the rating column and cleans the count by adding how many times the
# clients have said one of these words. The star ratting column will help identify weather 
# the Positive words were wrongly interpreted ,therefore, valued bad by the afinn lexicon library.
#               Understanding "words" when used in Context is important 
datt2<- datt %>% count(Username, Rating, Word)
##        What I learn from the new data frame is that Art F. and Carmen G were the only two to use
##        the word Recommend in their post in a negative context. Affin associates the word recommend as a positive value 
##        however they both used the word negatively. My discovery came from the Rating column 
##        which both customers gave a one star rating on Yelp which helped me conclude that they used the word in a negative context.
##        In the Negative group only bad and died were more likely used in a negative context because those words
##        were used by a customers who gave a one star rating. I will take a picture of both negative and 
##      positive groups and show what I found from all 6 words that scored high in both sentiment categories. Overall gblandscaping as 
##      a company operates well  with their customers and in return positive reviews are on his Yelp account. Future customers will frequently see 
##.     that recommend, Happy and Amazing are frequently said and future and new customers will work with him 
##      


