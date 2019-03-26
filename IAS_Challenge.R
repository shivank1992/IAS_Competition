##  Install packages
list.of.packages <- c("shiny","dplyr","DT","readr","readxl","tidyr","xlsx","wordcloud","stringr","formattable","caret","zoo","lubridate")
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
library(caret)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mov_budget.df <- read_excel("./IAC_4.0/IAC_4.0/IAC4-dataDescription.xlsx", 
                            sheet = "budget.tsv",
                            na=c("","N/A"),range = "A1:S8469")
mov_budget_gross.df <- read_excel("./IAC_4.0/IAC_4.0/IAC4-dataDescription.xlsx", 
                                  sheet = "budget_gross.tsv",
                                  na=c("","N/A"),range = "A1:C8468")
sentiment <- read.csv("./IAC_4.0/IAC_4.0/UMCSENT.csv")
##EDA
##Check for NA values
sapply(mov_budget.df,function(x){sum(is.na(x))})
sapply(mov_budget_gross.df,function(x){sum(is.na(x))})
# 
# #genre=aggregate(genre ~.,data=genre,paste,collapse=",") # remove duplicates 
# genre_count <- mov_budget.df %>% 
#   group_by(genre) %>% 
#     summarise(count=length(genre)) %>% 
#       arrange(desc(count))#A look at the genre variety in our dataset
# #wordcloud
# wordcloud(words=genre_count$genre,freq=genre_count$count,
#           min.freq=100,max.words = 20,random.order=FALSE,
#           random.color=FALSE,rot.per=0.35,
#           colors = brewer.pal(20,"Dark2"),scale=c(5,.2))
# 
# #Top 10 Movies
# mov_budget.df %>% select(title,Budget) %>% 
#   drop_na(title)%>% 
#     arrange(desc(Budget)) %>% 
#       head(10) %>%  
#         ggplot(aes(reorder(title,Budget),Budget,fill=title))+
#         geom_bar(stat="identity")+
#         theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Red",face="italic"),
#               legend.position="none")+
#         scale_y_continuous(labels=scales::comma)+
#         labs(x="",y="Total Budget in $",title="Most Expensive Movies -Top 10")

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


# a<- as.data.frame(str_split_fixed(mov_budget.df$actors, ", ", Inf))
# b<- as.data.frame(mov_budget.df %>% separate(actors, actor1, sep = ", ", remove = TRUE, convert = FALSE))
# 
# d<- mov_budget.df %>%
#   separate(director, into = c("director1", "director2"), sep=", ")
# 
# sapply(d,function(x){sum(is.na(x))})
# 
# 
# #Directors average rating. Only for directors who have >5 movies in our database
# d %>%
#   group_by(director1) %>%
#     summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#         arrange(desc(avg_rating)) %>%
#           top_n(30, avg_rating) %>%
#             formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# d %>%
#   group_by(director2) %>%
#     summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#       filter(Tot.films>5) %>%
#         arrange(desc(avg_rating)) %>%
#           top_n(30, avg_rating) %>%
#             formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# #Actors average rating. Only for actors that have >5 movies in our database
# mov_budget.df %>%
#   str_split_fixed(mov_budget.df$actors, ", ") %>%
#     group_by(str_split_fixed(mov_budget.df$actors, ", ", Inf)) %>%
#       summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#         filter(Tot.films>5) %>%
#           arrange(desc(avg_rating)) %>%
#             top_n(30, avg_rating) %>%
#               formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# 
# 
# a<- mov_budget.df %>%
#   separate(actors, into = c("actor1", "actor2","actor3","actor4"), sep=", ")
# 
# a %>%
#   group_by(actor1) %>%
#   summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#   filter(Tot.films>5) %>%
#   arrange(desc(avg_rating)) %>%
#   top_n(10, avg_rating) %>%
#   formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# a %>%
#   group_by(actor2) %>%
#   summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#   filter(Tot.films>5) %>%
#   arrange(desc(avg_rating)) %>%
#   top_n(10, avg_rating) %>%
#   formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# a %>%
#   group_by(actor3) %>%
#   summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#   filter(Tot.films>5) %>%
#   arrange(desc(avg_rating)) %>%
#   top_n(10, avg_rating) %>%
#   formattable(list(avg_rating = color_bar("orange")), align = 'l')
# 
# a %>%
#   group_by(actor4) %>%
#   summarise(Tot.films = n(), avg_rating = mean(movie_rating__1)) %>%
#   filter(Tot.films>5) %>%
#   arrange(desc(avg_rating)) %>%
#   top_n(10, avg_rating) %>%
#   formattable(list(avg_rating = color_bar("orange")), align = 'l')

#Remove zero values
glimpse(mov_budget.df)
movie <- mov_budget.df[mov_budget.df$`Box Office Gross`!= 0, ]

movie2 <- movie[movie$Budget!= 0, ]

hist(log10(movie2$Budget))

movie2$performance <- as.numeric(movie2$`Box Office Gross`)/movie2$Budget

movie3<- movie2 %>% filter(!Budget == 44)
movie4<- movie3 %>% filter(!Budget == 300)

movie4$gross <- as.numeric(movie4$`Box Office Gross`)
movie4<-  movie4[complete.cases(movie4$gross), ]

# #Top Actors highest budget
# movie4<- movie4 %>%
#   separate(actors, into = c("actor1", "actor2","actor3","actor4"), sep=", ")
# 
# movie4 %>%
#   group_by(actor1) %>%
#   summarise(Tot.films = n(), avg_gross = mean(gross)) %>%
#   arrange(desc(avg_gross)) %>%
#   top_n(10, avg_gross) %>%
#   formattable(list(avg_gross = color_bar("orange")), align = 'l')
# 
# movie4 %>% group_by(name) %>% 
#   tally() %>% arrange(desc(n)) %>% 
#   head(10) %>% 
#   ggplot(aes(factor(name,levels=name),n,fill=name))+
#   geom_bar(stat="identity")+
#   labs(x="Artist",y="Count",title="Top 10 artist with most movies")+
#   theme_few()+
#   theme(axis.text.x=element_text(angle=90),
#         plot.title=element_text(hjust=0.5,color="red"),legend.position="none")


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


#HighBudget vs Successful Table
table(movie4$highbudget,movie4$successful,
      dnn = c("HighBudget", "Successful"))

#Join sentiment with movie table
movie4$month <- format(as.Date(movie4$release_date), "%Y-%m")
sentiment$DATE <- strptime(as.character(sentiment$DATE), "%d/%m/%Y")
sentiment$DATE <- format(sentiment$DATE, "%Y-%m")
names(sentiment)[1] <- "month"

# z <- left_join(movie4, sentiment %>% rename(month=month), by="month")
# z$UMCSENT[is.na(z$UMCSENT)] <- mean(z$UMCSENT, na.rm=TRUE)
# 
# movie4<- z
# glimpse(movie4)
movie4$quarter <- as.yearqtr(movie4$month, format = "%Y-%m")
movie4$quarter <- format(movie4$quarter, format = "%q")

#change NA to zero for columns globe, awards, oscars


#match(c("wins","nomination","golden_globe","oscars"),names(movie4))

##columns
library(lubridate)
movie4$release_date <- strptime(movie4$release_date, format="%F")
movie4$dvd_release <- strptime(movie4$dvd_release, format="%F")

movie4 <- movie4[-which(is.na(movie4$release_date)),]

movie4$actors <- tolower(movie4$actors)
movie4$director <- tolower(movie4$director)

actors <- strsplit(movie4$actors,",")
directors <- strsplit(movie4$director,",")

act_max <- rep(0,nrow(movie4))
act_bud <- rep(0,nrow(movie4))
dir_max <- rep(0,nrow(movie4))
dir_bud <- rep(0,nrow(movie4))



for (i in 1:nrow(movie4)){
  max_amov <- rep(0,length(actors[[i]]))
  max_aavg <- rep(0,length(actors[[i]]))
  for (j in 1:length(actors[[i]])){
    sub_movie <- movie4[sapply(actors, function(x) actors[[i]][[j]] %in% x),]
    sub_movie <- subset(sub_movie, release_date <= movie4$release_date[i])
    max_amov[j] = nrow(sub_movie)
    max_aavg[j] = mean(sub_movie$Budget, na.rm = TRUE)
  }
  act_max[i] <- max(max_amov)
  act_bud[i] <- max(max_aavg)
  
  max_dmov <- rep(0,length(directors[[i]]))
  max_davg <- rep(0,length(directors[[i]]))
  for (j in 1:length(directors[[i]])){
    sub_movie <- movie4[sapply(directors, function(x) directors[[i]][[j]] %in% x),]
    sub_movie <- subset(sub_movie, release_date <= movie4$release_date[i])
    if(i==840){
      print(directors[[i]][[j]])
      mid_df=sub_movie
    }
    max_dmov[j] = nrow(sub_movie)
    max_davg[j] = mean(sub_movie$Budget, na.rm = TRUE)
  }
  dir_max[i] <- max(max_dmov)
  dir_bud[i] <- max(max_davg)  
}

movie4 <- cbind(act_max, as.data.frame(movie4))
movie4 <- cbind(act_bud, as.data.frame(movie4))
movie4 <- cbind(dir_max, as.data.frame(movie4))
movie4 <- cbind(dir_bud, as.data.frame(movie4))
glimpse(movie4)

###############################################################################################
#Start Data Modelling

##Data Prep
# Subset data based on Budget
lowbudget <- movie4[which(movie4$highbudget==0),]
highbudget <- movie4[which(movie4$highbudget==1),]

lowbudget$runtime <- str_extract(string = lowbudget$runtime, "\\d+\\b(?=\\smin)")
lowbudget$runtime <- as.numeric(as.character(lowbudget$runtime))

lowbudget$wins[is.na(lowbudget$wins)]=0
lowbudget$nomination[is.na(lowbudget$nomination)]=0
lowbudget$runtime[is.na(lowbudget$runtime)]=mean(lowbudget$runtime, na.rm = TRUE)
mov_budget.df$wins <- str_extract(string = mov_budget.df$awards, "\\d+\\b(?=\\swin)")
sapply(lowbudget,function(x){sum(is.na(x))})
lowbudget$wins <- as.numeric(as.character(lowbudget$wins))
lowbudget$nomination <- as.numeric(as.character(lowbudget$nomination))
lowbudget$quarter <- as.factor(as.character(lowbudget$quarter))

##Model for small budget
#split Data
set.seed(13)
lbtrainindex <- createDataPartition(lowbudget$successful, p=0.80, list= FALSE)
lb.tr <- lowbudget[lbtrainindex, ]
lb.te <- lowbudget[-lbtrainindex, ]

glimpse(lb.tr)


##Decision Tree
library(doParallel)

cl <- parallel::makeCluster(detectCores(logical=TRUE)-1, type='PSOCK')
doParallel::registerDoParallel(cl)
start.time <- Sys.time()
tune.gridcart <- expand.grid(maxdepth = seq(1,30,1))
trnControl <- trainControl(method='cv',number=5, allowParallel = TRUE)
set.seed(13)

# lb_dtree_fit_gini <- train(successful ~ runtime + wins + nomination + act_max + dir_max + sequel + quarter, data = lb.tr, method = "rpart2",
#                            parms = list(split = "information"),
#                            trControl=trnControl,
#                            tuneLength = 3,
#                            tuneGrid =tune.gridcart)

lb_dtree_fit_gini <- train(successful ~ runtime + act_max + dir_max + sequel + quarter, data = lb.tr, method = "rpart",
                           parms = list(split = "gini"),
                           trControl=trnControl,
                           tuneLength = 3)


fb_dtree_fit_gini_t<- Sys.time() - start.time
parallel::stopCluster(cl)
registerDoSEQ()


lb_dtree_fit_gini
plot(lb_dtree_fit_gini)
plot(lb_dtree_fit_gini$finalModel)
text(lb_dtree_fit_gini$finalModel)
library(rattle)
fancyRpartPlot(lb_dtree_fit_gini$finalModel, uniform=TRUE,
               main="Pruned Classification Tree")

glimpse(lb.tr)




lb_log_reg <- train(successful ~ runtime + act_max + dir_max + sequel + quarter, data = lb.tr, method = "glm",
                    family=binomial(),
                    trControl=trnControl,
                    tuneLength = 3)

lb_log_reg
plot(lb_log_reg)
plot(varImp(lb_log_reg))

cl <- parallel::makeCluster(detectCores(logical=TRUE)-1, type='PSOCK')
doParallel::registerDoParallel(cl)
lb_bdt <- train(successful ~ runtime + act_max + dir_max + sequel + quarter, data = lb.tr, method = "xgbTree",
                    tuneLength = 3)
parallel::stopCluster(cl)
registerDoSEQ()

lb_bdt
plot(lb_bdt)
plot(varImp(lb_bdt))
