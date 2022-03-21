###---OPTIONS AND PACKAGES---###

#Setting Libraries
library('ggplot2')
library('qdapRegex')
library('tm')
library('NLP')
library('stringi')
library('stringr')
library('ggthemes')


#setting the WD for the data
setwd("C:/Users/Jack B/Documents/hult/TXT analytics/Text-Mining-NLP/Case/Case I/Data")


#options for script
Sys.setlocale('LC_ALL','C') #In case some text is in different Languages


#UDF for cleaning data
cleandata<-function(data){
  data<- gsub('\\bRT\\b','' ,data)
  data<- gsub('@','' ,data)
  data<- tolower(data)
  data<- gsub("[^\x01-\x7F]", "", data)
  data<- gsub("#", "", data)
  data<-stripWhitespace(data)
  return(data)
}#closing the UDF


#UDF for re-Index Columns
redex<- function(x){
  row.names(x)<-NULL
  
  return(x)
}#closing the UDF


#Reading Data Files
may_data <- read.csv('H_May2020.csv', header=TRUE)
june_data <- read.csv('I_June2020.csv', header=TRUE)
july_data <- read.csv('J_July2020.csv', header=TRUE)

###---DATA ORGANIZATION/CLEANING---###


# selecting a random subset for each data file
may_data_subest_size<- sample(1:nrow(may_data),size = 4000)
may_sorted_data<-may_data[may_data_subest_size,]

june_data_subest_size<- sample(1:nrow(june_data),size = 4000)
june_sorted_data<-june_data[june_data_subest_size,]

july_data_subest_size<- sample(1:nrow(july_data),size = 4000)
july_sorted_data<-july_data[july_data_subest_size,]


#removing any NAN Data and renaming data object
may_sorted_data<-as.matrix(may_sorted_data,na.rm = TRUE)
june_sorted_data<-as.matrix(june_sorted_data,na.rm = TRUE)
july_sorted_data<-as.matrix(july_sorted_data,na.rm = TRUE)

#merge into one data object
combined_DF<- rbind(may_sorted_data,june_sorted_data,july_sorted_data)


#Applying the UDF to re-Index the files after rm.NAN
july_redex <- redex(july_sorted_data)
june_redex <- redex(june_sorted_data)
may_redex  <- redex(may_sorted_data)
combined_DF<- redex(combined_DF)

#Applying the UDF to clean the data files
may_data_clean<- cleandata(may_redex)
june_data_clean<- cleandata(june_redex)
july_data_clean<- cleandata(july_redex)

#transforming the clean data back to a DF
may_DF<-as.data.frame(may_data_clean)
june_DF<-as.data.frame(june_data_clean)
july_DF<-as.data.frame(july_data_clean)

#counting the time Nike is found in the tweets
nike_count_june   <- grepl("nike", may_DF$text)
nike_count_may    <- grepl("nike", june_DF$text)
nike_count_july   <- grepl("nike", july_DF$text)

#summarizing the frequency of Nike from tweets
nike_may<-table(nike_count_may)
nike_june<-table(nike_count_june)
nike_july<-table(nike_count_july)

###---DATA CALCULATIONS & ANALYSIS---###

sum_nike<- nike_may[2] +nike_june[2] +nike_july[2]
(sum_nike/12000)*100

table(nike_may)
may_DF$text[47]
sum(nike_may)

#eastern team conference
eastern_teams<- c("Milwaukee Bucks","Orlando Magic",
                 'Toronto Raptors',"Brooklyn Nets",
                 'Boston Celtics','philadelphia sixers',
                 'Indiana Pacers','Miami Heat')

#converting vector to lower char
eastern_teams<- tolower(eastern_teams)

#western team conference
western_teams<- c('la lakers','Portland Trail Blazers',
                  'la clippers','Dallas Mavericks',
                  'Denver Nuggets','Utah Jazz','Houston Rockets',
                  'Oklahoma City Thunder')

#converting vector to lower char
western_teams<- tolower(western_teams)


#EASTERN MAY
team_1<-  str_count(may_DF$team,pattern =eastern_teams[1])
team_2<-  str_count(may_DF$team,pattern =eastern_teams[2])
team_3<-  str_count(may_DF$team,pattern =eastern_teams[3])
team_4<-  str_count(may_DF$team,pattern =eastern_teams[4])
team_5<-  str_count(may_DF$team,pattern =eastern_teams[5])
team_6<-  str_count(may_DF$team,pattern =eastern_teams[6])
team_7<-  str_count(may_DF$team,pattern =eastern_teams[7])
team_8<-  str_count(may_DF$team,pattern =eastern_teams[8])
#WESTERN
team_1a<-  str_count(may_DF$team,pattern =western_teams[1])
team_2a<-  str_count(may_DF$team,pattern =western_teams[2])
team_3a<-  str_count(may_DF$team,pattern =western_teams[3])
team_4a<-  str_count(may_DF$team,pattern =western_teams[4])
team_5a<-  str_count(may_DF$team,pattern =western_teams[5])
team_6a<-  str_count(may_DF$team,pattern =western_teams[6])
team_7a<-  str_count(may_DF$team,pattern =western_teams[7])
team_8a<-  str_count(may_DF$team,pattern =western_teams[8])
#SUM
sum_west_may<-sum(table(team_1a)[2],table(team_2a)[2],table(team_3a)[2],table(team_4a)[2],table(team_5a)[2],table(team_6a)[2],table(team_7a)[2],table(team_8a)[2])
sum_east_may<-sum(table(team_1)[2],table(team_2)[2],table(team_3)[2],table(team_4)[2],table(team_5)[2],table(team_6)[2],table(team_7)[2],table(team_8)[2])
east_vs_west_tweets_may<-c(sum_east_may,sum_west_may)
east_vs_west_tweets_may
#EASTERN june
team_11<-  str_count(june_DF$team,pattern =eastern_teams[1])
team_21<-  str_count(june_DF$team,pattern =eastern_teams[2])
team_31<-  str_count(june_DF$team,pattern =eastern_teams[3])
team_41<-  str_count(june_DF$team,pattern =eastern_teams[4])
team_51<-  str_count(june_DF$team,pattern =eastern_teams[5])
team_61<-  str_count(june_DF$team,pattern =eastern_teams[6])
team_71<-  str_count(june_DF$team,pattern =eastern_teams[7])
team_81<-  str_count(june_DF$team,pattern =eastern_teams[8])
#WESTERN
team_1b<-  str_count(june_DF$team,pattern =western_teams[1])
team_2b<-  str_count(june_DF$team,pattern =western_teams[2])
team_3b<-  str_count(june_DF$team,pattern =western_teams[3])
team_4b<-  str_count(june_DF$team,pattern =western_teams[4])
team_5b<-  str_count(june_DF$team,pattern =western_teams[5])
team_6b<-  str_count(june_DF$team,pattern =western_teams[6])
team_7b<-  str_count(june_DF$team,pattern =western_teams[7])
team_8b<-  str_count(june_DF$team,pattern =western_teams[8])
#SUM
sum_west_june<-sum(table(team_1b)[2],table(team_2b)[2],table(team_3b)[2],table(team_4b)[2],table(team_5b)[2],table(team_6b)[2],table(team_7b)[2],table(team_8b)[2])
sum_east_june<-sum(table(team_11)[2],table(team_21)[2],table(team_31)[2],table(team_41)[2],table(team_51)[2],table(team_61)[2],table(team_71)[2],table(team_81)[2])
east_vs_west_tweets_june<-c(sum_east_june,sum_west_june)
east_vs_west_tweets_june
#EASTERN july
team_111<-  str_count(july_DF$team,pattern =eastern_teams[1])
team_211<-  str_count(july_DF$team,pattern =eastern_teams[2])
team_311<-  str_count(july_DF$team,pattern =eastern_teams[3])
team_411<-  str_count(july_DF$team,pattern =eastern_teams[4])
team_511<-  str_count(july_DF$team,pattern =eastern_teams[5])
team_611<-  str_count(july_DF$team,pattern =eastern_teams[6])
team_711<-  str_count(july_DF$team,pattern =eastern_teams[7])
team_811<-  str_count(july_DF$team,pattern =eastern_teams[8])
#WESTERN
team_1c<-  str_count(july_DF$team,pattern =western_teams[1])
team_2c<-  str_count(july_DF$team,pattern =western_teams[2])
team_3c<-  str_count(july_DF$team,pattern =western_teams[3])
team_4c<-  str_count(july_DF$team,pattern =western_teams[4])
team_5c<-  str_count(july_DF$team,pattern =western_teams[5])
team_6c<-  str_count(july_DF$team,pattern =western_teams[6])
team_7c<-  str_count(july_DF$team,pattern =western_teams[7])
team_8c<-  str_count(july_DF$team,pattern =western_teams[8])
#SUM
sum_west_july<-sum(table(team_1c)[2],table(team_2c)[2],table(team_3c)[2],table(team_4c)[2],table(team_5c)[2],table(team_6c)[2],table(team_7c)[2],table(team_8c)[2])
sum_east_july<-sum(table(team_111)[2],table(team_211)[2],table(team_311)[2],table(team_411)[2],table(team_511)[2],table(team_611)[2],table(team_711)[2],table(team_811)[2])
east_vs_west_tweets_july<-c(sum_east_july,sum_west_july)
east_vs_west_tweets_july

#creating totals for the teams from the tweets
total_tweets_playoffs<-sum(east_vs_west_tweets_july,east_vs_west_tweets_june,east_vs_west_tweets_may)
total_tweets<- nrow(may_DF)+nrow(june_DF)+nrow(july_DF)

#calculating the percent of total tweets that come from the playoff teams
playoff_team_tweet_ratio<- total_tweets_playoffs / total_tweets
round(playoff_team_tweet_ratio,2)*100 


#competitor comparison
nike_may    <- grepl("nike", may_DF$text)
nike_june    <- grepl("nike", june_DF$text)
nike_july   <- grepl("nike", july_DF$text)

adidas_may   <- grepl("adidas", may_DF$text)
adidas_june   <- grepl("adidas", june_DF$text)
adidas_july   <- grepl("adidas", july_DF$text)

adidas_total<- table(adidas_july)[2] + table(adidas_june)[2] + table(adidas_may)[2] 
nike_total<-   table(nike_july)[2] + table(nike_june)[2] + table(nike_may)[2]
adidas_vs_nike<-c(nike_total,adidas_total)


#Checking the count for each player mentioned in tweets
curry_may   <- grepl("steph curry|stephen curry", may_DF$text)
curry_june   <- grepl("steph curry|stephen curry", june_DF$text)
curry_july   <- grepl("steph curry|stephen curry", july_DF$text)

james_may   <- grepl("lebron james|lebron", may_DF$text)
james_june   <- grepl("lebron james|lebron", june_DF$text)
james_july   <- grepl("lebron james|lebron", july_DF$text)

shaq_may   <- grepl("shaq", may_DF$text)
shaq_june   <- grepl("shaq", june_DF$text)
shaq_july   <- grepl("shaq", july_DF$text)

#sum of the player mentions
curry_total<- sum(curry_july) + sum(curry_may) +sum(curry_june)
james_total<- sum(james_july) + sum(james_may) +sum(james_june)
shaq_total<- sum(shaq_july) + sum(shaq_may) +sum(shaq_june)

#saving sum as a vector 
total_mentions<- c(curry_total,james_total,shaq_total)



###---DATA VISUALIZATION---###


#plotting total mentions from NBA stars
df_chart2a <- data.frame(
  group = c("Curry|UA", "Lebron|Nike","Shaq|Rebook"),
  value = c(curry_total, james_total,shaq_total)
)
player<- c("Curry|UA", "Lebron|Nike","Shaq|Rebook")
player_mentions<-ggplot(df_chart2a, aes(x=player, y=total_mentions,fill = player)) + geom_bar(stat="identity") + scale_fill_manual(values=c("blue2", "purple",'blue3'))
player_mentions


#Plotting adidas VS Nike mentions 
df_chart2 <- data.frame(
  group = c("Adidas", "Nike"),
  value = c(adidas_total, nike_total)
)

# Bar/Pie chart for competition
bp<- ggplot(df_chart2, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
#Pie plot for Nike vs Adidas
pie <- bp + coord_polar("y", start=0)
pie


#plotting the count of Nike mentions per month
nike_overall_count<- c(sum(nike_count_may),sum(nike_count_june),sum(nike_count_july))

df_chart3 <- data.frame(
  group = c("May", "June","July"),
  value = nike_overall_count
)

nike_count_plot<-ggplot(df_chart3, aes(x=group, y=value,fill = group))+ geom_bar(stat="identity") + scale_fill_manual(values=c("blue2", "purple",'blue3'))

nike_count_plot





