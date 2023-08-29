# The purpose of this web scrapper was to extract content from Yelp 
# The research I conducted is sentimental analysis on a land Sccapping company 
# Once I completed the web scrapper I imported the new dataset to google Spread Sheets
# for further processing. 


# Set my working directory
setwd("/Users/hugolopez/Desktop/Data Machine Learnign ")


#Install the correct packages 
install.packages("rvest")


# load in the correct 
library(rvest)
library(RSelenium)
library(tidyverse)
library(data.table)
library(rlist)

# Load in the URL from yelp to a url object

url = "https://www.yelp.com/biz/gb-landscaping-services-ontario?sort_by=date_desc"

# convert URL to HTML object 
page <- read_html(url)

# DF Final 
df_final <- list()

# PageNums 
pageNums <- page %>% 
  html_elements(xpath = "//div[@aria-label= 'Pagination navigation']") %>%
  html_text() %>% 
  str_extract('of \\d+') %>% 
  str_remove('of') %>% 
  as.numeric()

# PageSequence
pageSequence <- seq(from = 0, to = (pageNums * 10 )-10,by =10)

#Run Everything in the Loop to get what I want.
for (i in pageSequence) {
  url = sprintf("https://www.yelp.com/biz/gb-landscaping-services-ontario?start=%d&sort_by=date_desc", i)
# This object reads the HTML script from Yelp 
  page <- read_html(url)
# For usernames I created a column to fit all client names who posted a review and
  # I did that to all of the content I targeted from the Yelp page. Username, Locations, Comments, Ratings, The_dates, Extra info 
  Usernames <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//a[starts-with(@href, '/user_details')]") %>%
    html_text()
  
  Locations <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//span[@class=' css-qgunke']") %>%
    html_text() %>%
    .[.!= "Location"]
  
  Comments <- page %>% 
    html_elements(xpath = "// div[starts-with(@class, ' margin-b2')]") %>%
    html_elements(xpath = "(.//span[starts-with(@class, ' raw')])[1]") %>% 
    html_text()
  
  Ratings <- page %>% 
    html_elements(xpath = "//div[starts-with(@class, ' margin-t1')]") %>%
    html_elements(xpath = "(.//div[contains(@aria-label, ' star rating')])[1]") %>%
    html_attr("aria-label") %>% 
    str_remove_all("star rating") %>% 
    as.numeric()
  
  The_dates <- page %>% 
    html_elements(xpath = "//div[starts-with(@class, ' margin-t1')]") %>% 
    html_elements(xpath = "(.//span[@class = ' css-chan6m'])[1]") %>% 
    html_text()
  
  
  # I used the Length function to fix the differing number of rows
  length(Ratings) <- length(Usernames)
  length(The_dates) <- length(Usernames)
  
  Extra_info <- page %>% 
    html_elements(xpath = "//div[starts-with(@class, ' arrange')]") %>%
    html_elements(xpath = ".//button[@type = 'submit']") %>%
    html_text() %>% 
    .[.!= ""] %>% 
    .[.!= "21 More Cost Guides"] %>% 
    .[.!= "Edit business info"] %>% 
    .[.!= "Edit"] %>% 
    .[.!= "Share"] %>% 
    .[.!= "Follow"] %>%
    .[.!= "Save"]
  
  Extra_info_extract <- function(ei, txt) {
    str_extract(ei, paste0(txt, ".*")) %>% 
      .[!is.na(.)] %>% 
      str_extract("\\d+") %>% 
      str_replace_na("0") %>% 
      as.numeric()
  }
  
  Useful <- Extra_info_extract(Extra_info, "Useful")
  Funny <- Extra_info_extract(Extra_info, "Funny")
  Cool <- Extra_info_extract(Extra_info, "Cool")
  df_new <- list(Username = Usernames,
                 Date = The_dates,
                 Location = Locations,
                 Rating = Ratings,
                 Comment = Comments,
                 Useful = Useful,
                 Funny = Funny,
                 Cool = Cool)
  
  df_new_tabble <- as.data.frame(df_new)
  
  df_final <- rbindlist(list(df_final, df_new_tabble))
  
# The final code is not to get myself in trouble with Extracting the information 
# to quickly from the Yelp Page
  Sys.sleep(sample(c(15,25), 1))
}

#  This goes to the working directory of Machine learning folder 
write_csv(df_final, "GB_LandScaping_Yelp_Review.csv", na = "")


# change the data set name gb_data frame
gb_dataframe <- df_final