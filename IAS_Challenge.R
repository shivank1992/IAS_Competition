##  Install packages
list.of.packages <- c("shiny","dplyr","DT","readr","readxl","tidyr","xlsx","wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# 

library(shiny)
library(dplyr)
library(DT)
library(readr)
library(readxl)
library(tidyr)
library(xlsx)
library(ggplot2)
library(wordcloud) #wordcloud

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


