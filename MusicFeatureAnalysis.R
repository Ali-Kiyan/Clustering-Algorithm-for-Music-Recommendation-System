#installing packages
#for string manipulation
install.packages("stringr")
#for k-means clustering algorithm
install.packages("cluster")
#for filetring based on string in datasets 
install.packages("dplyr")
#using respective packages
library(stringr) 
library(cluster)
library(dplyr)
#reading data set 
music <- read.csv("music.csv", header=T) 
#selecting musiclly significant features for analysis
music_cleaned <- music[,c(1, 7, 9, 10, 11, 18, 24,26, 28, 29, 30, 32)]
#check if there is any NA in the dataset 
any(is.na(music_cleaned))
#checking the structure and dimention of the dataset 
dim(music_cleaned)
str(music_cleaned)
#getting the number  of NAs 
sum(is.na(music_cleaned$song.hotttnesss))
#export to a csv file 
write.csv(music_cleaned, file='musicCleand.csv')
#checking the number of missing values for each column
sum(is.na(music_cleaned$artist.hotttnesss))
sum(is.na(music_cleaned$bars_start))
sum(is.na(music_cleaned$beats_start))
sum(is.na(music_cleaned$duration))
sum(is.na(music_cleaned$end_of_fade_in))
sum(is.na(music_cleaned$loudness))
sum(is.na(music_cleaned$song.hotttnesss))
sum(is.na(music_cleaned$start_of_fade_out))
sum(is.na(music_cleaned$tatums_start))
sum(is.na(music_cleaned$tempo))
sum(is.na(music_cleaned$time_signature))
sum(is.na(music_cleaned$terms))

#song popularity histogram
hist(music_cleaned$song.hotttnesss ,col=c(1,2,3,4,5,6,7,8,9,10))

#using stringr and dplyr retrun a separate dataset if the track has one of the 5 main genre type in its terms column 
#Rock dataset
#contains_rock <- music_cleaned %>% 
#  filter(str_detect(music_cleaned$terms, "rock"))
#nrow(contains_rock)
#Jazz dataset
contains_Jazz <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "jazz"))
nrow(contains_Jazz)
#Pop dataset 
#contains_Pop <- music_cleaned %>% 
#  filter(str_detect(music_cleaned$terms, "pop"))
#nrow(contains_Pop)
#classic dataset
contains_Classic <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "classic"))
nrow(contains_Classic)
#country dataset
#contains_Country <- music_cleaned %>% 
#  filter(str_detect(music_cleaned$terms, "country"))
#nrow(contains_Country)

#calculate the mean for each main genres 


#Rock_Song_hotness_mean <- mean(contains_rock$song.hotttnesss, na.rm = TRUE)


Jazz_Song_hotness_mean <- mean(contains_Jazz$song.hotttnesss, na.rm = TRUE)


#Pop_Song_hotness_mean <- mean(contains_Pop$song.hotttnesss, na.rm = TRUE)


Classic_Song_hotness_mean <- mean(contains_Classic$song.hotttnesss, na.rm = TRUE)


#Country_Song_hotness_mean <- mean(contains_Country$song.hotttnesss, na.rm = TRUE)


#3.impute the mean to every NA row
length <- nrow(music_cleaned)


#for Rock Tracks 
#for(i in 1:length) 
#{
#  if(is.na(music_cleaned$song.hotttnesss[i]))
#  {
#    if(grepl("rock", music_cleaned$terms[i]))
#      {
#         music_cleaned$song.hotttnesss[i] <- Rock_Song_hotness_mean
#      }
#  }
#}
#remaining NA rows 
#sum(is.na(music_cleaned))
#for Jazz Tracks
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("jazz", music_cleaned$terms[i]))
    {
      music_cleaned$song.hotttnesss[i] <- Jazz_Song_hotness_mean
    }
  }
}
#remaining NA rows 
sum(is.na(music_cleaned))
#for POP Tracks

#for(i in 1:length) 
#{
#  if(is.na(music_cleaned$song.hotttnesss[i]))
#  {
#    if(grepl("pop", music_cleaned$terms[i]))
#    {
#     music_cleaned$song.hotttnesss[i] <- Pop_Song_hotness_mean
#    }
#  }
#}
#remaining NA rows 
#sum(is.na(music_cleaned))
#for Classical Tracks
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("classic", music_cleaned$terms[i]))
    {
      music_cleaned$song.hotttnesss[i] <- Classic_Song_hotness_mean
    }
  }
}
#remaining NA rows 
sum(is.na(music_cleaned))
#for country Tracks
#for(i in 1:length) 
#{
#  if(is.na(music_cleaned$song.hotttnesss[i]))
#  {
#    if(grepl("country", music_cleaned$terms[i]))
#    {
#      music_cleaned$song.hotttnesss[i] <- Country_Song_hotness_mean
#    }
#  }
#}
#remaining NA rows 
#sum(is.na(music_cleaned))
#getting the number  of NAs 
sum(is.na(music_cleaned$song.hotttnesss))


#eliminating NA left in  the dataset
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
#removing zeros in the rows
#music_cleaned <- music_cleaned[-which(music_cleaned$song.hotttnesss == 0),] 
#using subset to eliminate zeros (alternative to perivious line)
#music_cleaned <- subset(music_cleaned,song.hotttnesss!=0)
#num of complete case (without NA)
sum(as.numeric(complete.cases(music_cleaned)))

head(music_cleaned)
#getting the pair of clusters
pairs(music_cleaned)
#scatter plot for a sample attribute
plot(music_cleaned$song.hotttnesss~ music_cleaned$tempo, data= music_cleaned)
#adding the terms(genre) --a lot of calculation
with(music_cleaned,text(music_cleaned$song.hotttnesss~ music_cleaned$artist.hotttnesss, labels=music_cleaned$terms,pos=4,cex=.6))
#removing the nominal value
music_cleaned <- music_cleaned[,-c(11)]

#normalizing with range between -1 and 1 (for more accurate)
col <- ncol(music_cleaned)
newmin = -1
newmax= 1
#initializing
nrml_music <- 0
#normalizing
for(j in 1:col){
  oldmin <- min(music_cleaned[j])
  oldmax <- max(music_cleaned[j])
  current <- music_cleaned[j]
  temp <- ((current-oldmin)/(oldmax-oldmin))*(newmax-newmin) + newmin
  temp <- as.data.frame(temp)
  nrml_music <- cbind(nrml_music, data.frame(temp))
}
#removing extra columns
nrml_music <- nrml_music[,-c(1)]
#check if the range is correct
min(nrml_music$song.hotttnesss)
max(nrml_music$song.hotttnesss)
#standardisation with substraction of mean and division by standard deviation with scale function (as an alternative to normalization)
#less accurate in this case
#means = apply(music_cleaned, 2, mean)
#sds = apply(music_cleaned, 2, sd)
#nrml_music = scale(music_cleaned, center = means, scale=sds)
#calculating the distance matrix
distance = dist(nrml_music)
#clustring the normalized dataset into three cluster with k-means package
kc <- kmeans(nrml_music,4)

#center of each cluster
kc$centers

#distribution of terms(genre in clusters)
table(music_cleaned$tempo, kc$cluster)

#results
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$tempo, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$end_of_fade_in, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$start_of_fade_out, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$tatums_start, data = music_cleaned,col=kc$cluster)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, data = music_cleaned,col=kc$cluster)




