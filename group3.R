
rm(list = ls())

### Keywords ###

# gay, same-sex, homosexual, lesbian, lgbt
# adoption, marriage, partnership, union


guardian_api_key<-"XX"
library(httr)
library(tidyverse)
library(jsonlite)



#### Test ####
base <- 'http://content.guardianapis.com/search'

params <- list('api-key' = guardian_api_key, # API key
               'page-size' = 50, # number of results
               'show-fields' = 'bodyText', #fetching the article main text as well [without this parameter we'd get only headlines]
               'q' = 'gay OR same-sex OR homosexual OR lgbt OR lesbian')

guardian_request <- GET(base, query = params)
guardian_request$status_code

json_data <- fromJSON(content(guardian_request, as = "text"))
df_test <- json_data$response$results

df$sectionName
df$sectionId
df$webPublicationDate

json_data$response$currentPage # current page
json_data$response$pageSize # items per request
mpages <- json_data$response$pages # total number of pages
mpages
# -> seems that we have nearly 1186 pages with a lot of articles




#### Broader sample ####

get_guardian_articles <- function(query, max_pages = 5, section='uk-news') {
        # parameters
        base <- 'http://content.guardianapis.com/search'
        params <- list(
                'api-key' = guardian_api_key,
                'show-fields' = 'bodyText',
                'section' = section,
                'page-size' = 50,
                'page' = 1,
                'q' = query
        )
        # first request
        req <- GET(base, query = params)
        data <- fromJSON(content(req, 'text'))
        df <- data$response$results
        # remaining pages
        for (page in 2:data$response$pages) {
                if (page <= max_pages) {
                        params$page <- page
                        req <- GET(base, query = params)
                        data <- fromJSON(content(req, 'text'))
                        df <- bind_rows(df, data$response$results)
                }
        }
        return(df)
}


# here we collect all articles -> takes time
# oldest articles in Opinion section were published in 2006
df1<-get_guardian_articles(query="gay OR same-sex OR homosexual OR lgbt OR lesbian", section="society", max_pages = mpages)
df<-get_guardian_articles(query="gay OR same-sex OR homosexual OR lgbt OR lesbian", section="commentisfree", max_pages = mpages)
df3<-get_guardian_articles(query="gay OR same-sex OR homosexual OR lgbt OR lesbian", section="politics", max_pages = mpages)
df4<-get_guardian_articles(query="gay OR same-sex OR homosexual OR lgbt OR lesbian", section="news", max_pages = mpages)
df5<-get_guardian_articles(query="gay OR same-sex OR homosexual OR lgbt OR lesbian", section="uk-news", max_pages = mpages)

df_all <- rbind.data.frame(df1,df,df3,df4,df5)


library(stringr)
df <- do.call(data.frame, df_all)

listsplit <- str_split(df$bodyText," ")
df$countcolumn <- sapply(listsplit,length)
View(head(df))


# you can use this column to normalize the sentiment counts for article length
#df$sent_norm <- df$sent/df$countcolumn

# df_all= data frame from 16/09
# df = data frame from 17/09

save(df,file = "df.R")
View(head(df_all))

nrow(df) 
head(df)
colnames(df)
#success!



### Step Nr. 2: Descriptive Exploration ###

library(data.table)
library(quanteda)
library(quanteda.textstats)
library(syuzhet)
library(dplyr)
library(topicmodels)


# import
load("df_all.R")
df <- do.call(data.frame, df_all)
getwd()

# define relevant variables #
df$year <- substr(df$webPublicationDate, 1, 4)
df$year
df$year_num <- as.numeric(df$year) #otherwise aggregation won't work#


# exclude all articles from before 1999, because online archive not fully availalble
df_red <- df %>% filter (df$year_num>1998)

df_red$year <- substr(df_red$webPublicationDate, 1, 4)
df_red$year
df_red$year_num <- as.numeric(df_red$year) #otherwise aggregation won't work#

df_red$title <- df_red$webTitle
df_red$text <- df_red$fields.bodyText
df_red$section <-df_red$sectionId


# Frequency of Article, over years
pl1 <- hist(year_num, 
            breaks = length(unique(year_num)),
            main = "Number of Articles per Year",
            ylab = "Number of Articles",
            xlab = "Publication Year",)
#looks good, largest peak round the early 2000's, smaller peak around 2013


# now plot number of articles over time, by sections, using ggplot
table(df$y)


## graph frequencies of articles by section over time ##
# Thomas' Code #
group_by(year, sectionName) %>% 
  summarise(numyear = n())

ggplot(ny, aes(x=year, y=numyear, color=sectionName)) +
  geom_line()

# Ines' code#
df_group <- df_red %>% group_by(year_num, section) %>% summarise(count=n())
df_group$section <- recode(df_group$section, "commentisfree" = "Opinion",
                           "news" = "News", "politics" = "Politics", "society"="Society",
                           "uk-news"="UK-News")
pl3 <- ggplot(df_group, aes(x = year_num, y = count, color=section)) + 
  geom_line(size = 1.25) + 
  theme_bw() +
  scale_color_manual(values = c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C","#08306B")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "solid", "dotted")) +
  labs(x = "Year", y = "Number of articles", title="Number of Articles per Year and Section")
png(filename = "freq_art.png",width = 800, height = 800)
pl3
dev.off()

df_lines <- data.frame(
  linetype = factor(
    1:5,
    labels = c("solid", "solid", "dashed", "solid", "dotted")
  )
)



### Preparation for Step 3 (Sentiment Analyis) ###
# define corpus #
Sys.setlocale("LC_ALL","English")

# Preparing corpus objects ------------------------------------------------
df$year <- substr(df$webPublicationDate, 1, 4)
df$month <- substr(df$webPublicationDate, 6, 7)

lemma_data <- read.csv("baseform_en.csv", encoding = "UTF-8")

class(df1[,8])
# there's a data frame nested within our data frame - let's unpack it! :-) 
df1 <- do.call(data.frame, df)
class(df1$bodyText)

df1 <- df1[df1$year > 1998,]
names(df1)

dfcorpus <- corpus(df1$bodyText)%>% tokens(.,remove_punct=TRUE,remove_numbers=TRUE,remove_symbols = T) %>%
  tokens_tolower() %>% tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>%
  tokens_select(., pattern = stopwords("en"), selection = "remove")

class(dfcorpus)
head(dfcorpus)


# Turning tokens-object into a data frame ---------------------------------

#first, we take the tokens-object and turn it into articles, then save them in a list
whatever <- lapply((dfcorpus),function(x) paste(x,collapse=" ")) 
#now, we take the list and turn it into a df
whate <- as.data.frame(do.call(rbind,whatever))
#here, Im taking the variables from the input data frame 
part1 <- df1[, c("id", "sectionName", "year")]
df_all_clean <- cbind(part1, whate)
#here im renaming the variable containing the articles
df_all_clean <- rename(df_all_clean,
                       bodyText = V1)

path3 <- paste0(path, "/", "df_all_clean.r")
save(df_all_clean, file =path3)



### Step 3 ###
#### Get the sentiment values ####

# getting the sentiment values
df$sentiment <- get_sentiment(df$bodyText, method="afinn")

# normalize sentiments
df$sentiment_norm <- df$sentiment/df$countcolumn

# lets have a look -> looks good ;)
df$sentiment[1]
df$sentiment_norm[1]
df$bodyText[1]

df$sentiment[2]
df$sentiment_norm[2]
df$bodyText[2]

summary(df$sentiment)
summary(df$sentiment_norm)

# create dateframe with sentiments and standardized sentiments
df_sentiment <- df
save(df,file = "df_sentiment.R")





#### Preparing data for the plots ####

# mean sentiment per year and section
normsentiments_by_year <- df %>% 
  group_by(year) %>% 
  summarise(sentiment_norm = mean(sentiment_norm))

save(normsentiments_by_year,file = "normsentiments_by_year.R")


# mean sentiment per year and section
# all articles #
normsentiments_by_year_and_section <- df %>% 
  group_by(year, sectionName) %>% 
  summarise(sentiment_norm = mean(sentiment_norm))

save(normsentiments_by_year_and_section,file = "normsentiments_by_year_and_section.R")


# same for articles with specific words: partnership OR marriage OR adoption OR union
toMatch <- c("partnership", "marriage", "adoption", "partnerships", "marriages", "adoptions", "union",
             "Partnership", "Marriage", "Adoption", "Partnerships", "Marriages", "Adoptions", "Union")
df_r <- df_sentiment %>%
  mutate(filter_term = ifelse(grepl(paste(toMatch,collapse="|"), bodyText), 1, 0)) %>% 
  filter(filter_term==1) 


table(df$sectionName)
table(df_r$sectionName)


normsenti_by_year_section_keywords <- df_r %>% 
  group_by(year, sectionName) %>% 
  summarise(sentiment_norm = mean(sentiment_norm))

save(normsenti_by_year_section_keywords,file = "normsenti_by_year_section_keywords.R")



### Step 4: Plotting ! ###
## First: Frequency of Terms in Subset of Articles ##
rm(list = ls())

load("freq_terms_df.R")
freq_df$year <- as.numeric(freq_df$year)

freq_new <- freq_df %>% pivot_longer(c(partnership,marriage,adoption,-year),names_to = "Terms",values_to = "Count")
freq_new$Terms <- recode(freq_new$Terms, "partnership"="Partnership","marriage"="Marriage","adoption"="Adoption")

freq_terms <- ggplot(data=freq_new, aes(year,Count,color=Terms)) +
  geom_line(size = 1.25) +
  theme_bw() +
  scale_color_manual(values = c("#08519C", "#4292C6", "#9ECAE1")) +
  labs(x = "Year", y = "Number of terms", title = "Number of Terms per Year")
png(filename = "freq_terms.png",width = 600, height = 600)
freq_terms
dev.off()


library(RColorBrewer)
brewer.pal(n = 9, name = "Blues")
brewer.pal(n = 5, name = "Blues")


##################

rm(list = ls())

load("normsentiments_by_year_subsample.R")

ggplot(normsentiments_by_year_subsample, aes(year,sentiment_norm)) +
  geom_line(size = 1.25, colour="#08519C") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  labs(x = "Year", y = "Average sentiment (normalized)", title = "Average Sentiment in the Opinion Section")

#################
rm(list = ls())


load("normsenti_by_year_section_keywords.R")

ggplot(normsenti_by_year_section_keywords, aes(year,sentiment_norm)) +
  geom_line(size = 1.25, colour="#08519C") +
  scale_x_continuous(name="Year", breaks=c(2000,2005,2010,2015,2020)) +
  facet_wrap("sectionName") +
  theme_bw() +
  labs(x = "Year", y = "Average sentiment (normalized)", title = "Average Sentiment per Section over Years")

new_df <- normsenti_by_year_section_keywords %>% filter(sectionName!="News")

ggplot(new_df, aes(year,sentiment_norm)) +
  geom_line(size = 1.25, colour="#08519C") +
  scale_x_continuous(name="Year", breaks=c(2000,2005,2010,2015,2020)) +
  facet_wrap("sectionName") +
  theme_bw() +
  labs(x = "Year", y = "Average sentiment (normalized)", title = "Average Sentiment per Section over Years")
