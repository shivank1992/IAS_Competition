##  Install packages
list.of.packages <- c("shiny","dplyr","DT","readr","readxl","tidyr","xlsx","wordcloud","stringr","formattable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# 


library(dplyr)
library(DT)
library(readr)
library(readxl)
library(tidyr)
library(xlsx)
library(ggplot2)
library(wordcloud) #wordcloud
library(stringr)
library(formattable)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mov_budget.df <- read_excel("./IAC_4.0/IAC_4.0/IAC4-dataDescription.xlsx", 
                            sheet = "budget.tsv",
                            na=c("","N/A"),range = "A1:S8469")
mov_budget_gross.df <- read_excel("./IAC_4.0/IAC_4.0/IAC4-dataDescription.xlsx", 
                                  sheet = "budget_gross.tsv",
                                  na=c("","N/A"),range = "A1:C8468")

##EDA
##Check for NA values
sapply(mov_budget.df,function(x){sum(is.na(x))})
sapply(mov_budget_gross.df,function(x){sum(is.na(x))})

#genre=aggregate(genre ~.,data=genre,paste,collapse=",") # remove duplicates 
genre_count <- mov_budget.df %>% 
  group_by(genre) %>% 
    summarise(count=length(genre)) %>% 
      arrange(desc(count))#A look at the genre variety in our dataset
#wordcloud
wordcloud(words=genre_count$genre,freq=genre_count$count,
          min.freq=100,max.words = 20,random.order=FALSE,
          random.color=FALSE,rot.per=0.35,
          colors = brewer.pal(20,"Dark2"),scale=c(5,.2))

#Top 10 Movies
mov_budget.df %>% select(title,Budget) %>% 
  drop_na(title)%>% 
    arrange(desc(Budget)) %>% 
      head(10) %>%  
        ggplot(aes(reorder(title,Budget),Budget,fill=title))+
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),
              legend.position="none")+
        scale_y_continuous(labels=scales::comma)+
        labs(x="",y="Total Budget in $",title="Most Expensive Movies -Top 10")

#Extract no of awards won per award category by each movie
mov_budget.df$awards<- tolower(mov_budget.df$awards)
mov_budget.df$wins <- str_extract(string = mov_budget.df$awards, "\\d+\\b(?=\\swin)")
mov_budget.df$nomination <- str_extract(string = mov_budget.df$awards, "\\d+\\b(?=\\snomination)")
mov_budget.df$oscars <- str_extract(string = mov_budget.df$awards, "\\d+\\b(?=\\soscar)")
mov_budget.df$golden_globe <- str_extract(string = mov_budget.df$awards, "\\d+\\b(?=\\sgolden globe)")

#Create dummy variable for sequel
mov_budget.df$sequel <- ifelse(grepl("sequel",mov_budget.df$keywords),yes=1,no=0)


#mov_budget <- mov_budget.df[complete.cases(mov_budget.df), ]
#dim(mov_budget)


a<- as.data.frame(str_split_fixed(mov_budget.df$actors, ", ", Inf))
b<- as.data.frame(mov_budget.df %>% separate(actors, actor1, sep = ", ", remove = TRUE, convert = FALSE))

d<- mov_budget.df %>%
  separate(director, into = c("director1", "director2"), sep=", ")

sapply(d,function(x){sum(is.na(x))})


#Directors average rating. Only for directors who have >5 movies in our database
d %>%
  group_by(director1) %>%
    summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
        arrange(desc(avg_rating)) %>%
          top_n(30, avg_rating) %>%
            formattable(list(avg_rating = color_bar("orange")), align = 'l')

d %>%
  group_by(director2) %>%
    summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
      filter(Tot.films>5) %>%
        arrange(desc(avg_rating)) %>%
          top_n(30, avg_rating) %>%
            formattable(list(avg_rating = color_bar("orange")), align = 'l')

#Actors average rating. Only for actors that have >5 movies in our database
mov_budget.df %>%
  str_split_fixed(mov_budget.df$actors, ", ") %>%
    group_by(str_split_fixed(mov_budget.df$actors, ", ", Inf)) %>%
      summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
        filter(Tot.films>5) %>%
          arrange(desc(avg_rating)) %>%
            top_n(30, avg_rating) %>%
              formattable(list(avg_rating = color_bar("orange")), align = 'l')



a<- mov_budget.df %>%
  separate(actors, into = c("actor1", "actor2","actor3","actor4"), sep=", ")

a %>%
  group_by(actor1) %>%
  summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
  filter(Tot.films>5) %>%
  arrange(desc(avg_rating)) %>%
  top_n(10, avg_rating) %>%
  formattable(list(avg_rating = color_bar("orange")), align = 'l')

a %>%
  group_by(actor2) %>%
  summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
  filter(Tot.films>5) %>%
  arrange(desc(avg_rating)) %>%
  top_n(10, avg_rating) %>%
  formattable(list(avg_rating = color_bar("orange")), align = 'l')

a %>%
  group_by(actor3) %>%
  summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
  filter(Tot.films>5) %>%
  arrange(desc(avg_rating)) %>%
  top_n(10, avg_rating) %>%
  formattable(list(avg_rating = color_bar("orange")), align = 'l')

a %>%
  group_by(actor4) %>%
  summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
  filter(Tot.films>5) %>%
  arrange(desc(avg_rating)) %>%
  top_n(10, avg_rating) %>%
  formattable(list(avg_rating = color_bar("orange")), align = 'l')

#Remove zero values
glimpse(mov_budget.df)
movie <- mov_budget.df[mov_budget.df$`Box Office Gross`!= 0, ]

movie2 <- movie[movie$Budget!= 0, ]

hist(log10(movie2$Budget))

movie2$performance <- as.numeric(movie2$`Box Office Gross`)/movie2$Budget

movie3<- movie2 %>% filter(!Budget == 44)
movie4<- movie3 %>% filter(!Budget == 300)

#Split into High Budget and Low Budget
movie4$highbudget <- cut(as.numeric(movie4$Budget), 
                         breaks = c(0, 5000000, Inf), 
                         labels = c(0,1))

##Count Plot
movie4 %>%
  ggplot(aes(highbudget))+
    geom_bar(color = "black", fill = "#00CCFF")+
      theme_minimal()+
        labs(title='Count Plot of Low Budget / High Budget')


#Split into Successful and Non-Successful
movie4$successful <- cut(as.numeric(movie4$performance), 
                         breaks = c(0, 1, Inf), 
                         labels = c(0,1))

##Count Plot
movie4 %>% na.omit() %>%
  ggplot(aes(successful))+
  geom_bar(color = "black", fill = "#00CCFF")+
  theme_minimal()+
  labs(title='Count Plot of Non-Sucessful / Successful')
