install.packages("stringr")
install.packages("cluster")
install.packages("dplyr")
library(stringr)
library(cluster)
library(dplyr)
music <- read.csv("music.csv", header=T)
music_cleaned <- music[,c(1, 7, 9, 10, 11, 18, 24,26, 28, 29, 30, 32)]
#check if there is any NA in the dataset 
any(is.na(music_cleaned))
dim(music_cleaned)
#number  of NAs 
sum(is.na(music_cleaned$song.hotttnesss))
#removing the empty values
dim(music_cleaned)
d <- music_cleaned[which(music_cleaned == 0),] 

u <- music_cleaned[1:5,]
u <- subset(u,u$song.hotttnesss !=0)
complete.cases(u)
na.omit(u)
is.na(u)
u$song.hotttnesss <- 1
length <- nrow(u)
for(i in 1:length){
  if(is.na(u$song.hotttnesss[i]))
  {
    u$song.hotttnesss[c(1,2,3)] <- 0
  }
}
for(i in 1:length){
  if(u$song.hotttnesss[i] == 0)
  {
    u$song.hotttnesss[i] <- NA
  }
}
for(i in 1:length){
  if(music_cleaned$terms[i] == "pop")
  {
    u$song.hotttnesss[i] <- 0
    
  }
}






#getting all the rows that contains pop (pop genre representative)
u_bar <- u %>% 
filter(str_detect(u$terms, "pop"))
l <- nrow(u_bar)
for(j in 1:l){
  if(is.na(u_bar$song.hotttnesss[j]))
  {
    u_bar$song.hotttnesss[j] <- 0
    
  }
}
sum(u_bar$song.hotttnesss)




#export the file to a csv file
write.csv(music_cleaned, file='musicCleand.csv')
#checking if any column has missing value
sum(is.na(music_cleaned$bars_start))
sum(is.na(music_cleaned$beats_start))
sum(is.na(music_cleaned$duration))
sum(is.na(music_cleaned$end_of_fade_in))
sum(is.na(music_cleaned$loudness))
sum(is.na(music_cleaned$start_of_fade_out))
sum(is.na(music_cleaned$tatums_start))
sum(is.na(music_cleaned$tempo))
sum(is.na(music_cleaned$song.hotttnesss))
sum(is.na(music_cleaned$artist.hotttnesss))
#song histogram
hist(music_cleaned$song.hotttnesss)
#getting the number of rows
length <- nrow(music_cleaned)
genre <- 0
remove(genre)
#1.if a row was missing check its genre 
for(i in 1:length)
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {

  }
}
genre <- genre[-c(1),]
genre <- data.frame(genre)
#1.1 if the terms col has pop, rock, jazz,classic, country 

#2.calc the mean for all the genras 
Rock_Song_hotness_mean
contains_rock <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "rock"))



Jazz_Song_hotness_mean
Pop_Song_hotness_mean
Classic_Song_hotness_mean

#2.1 check t
#3.impute the mean to every missing value 

#if a row wass zero in song hotness remove it (meaningless) note: it does not consider NAs as well
music_cleaned <- subset(music_cleaned,song.hotttnesss!=0)
#making sure there is no NA left in  the dataset
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
#num of complete case (without NA)
sum(as.numeric(complete.cases(music_cleaned)))


dim(music_cleaned)
library(cluster)
head(music_cleaned)
#getting the pair of clusters
pairs(music_cleaned)
#scatter plot for a sample attribute
plot(music_cleaned$song.hotttnesss~ music_cleaned$tempo, data= music_cleaned)
#adding the terms(genre) --a lot of calculation
with(music_cleaned,text(music_cleaned$song.hotttnesss~ music_cleaned$artist.hotttnesss, labels=music_cleaned$terms,pos=4,cex=.6))
#removing the nominal value
music_cleaned <- music_cleaned[,-c(11)]

#normalizing with range between -1 and 1
col <- ncol(music_cleaned)
newmin = -1
newmax= 1
str(nrml_music) 
nrml_music <- 0
for(j in 1:col)
{
  
  oldmin <- min(music_cleaned[j])
  oldmax <- max(music_cleaned[j])
  current <- music_cleaned[j]
  temp <- ((current-oldmin)/(oldmax-oldmin))*(newmax-newmin) + newmin
  temp <- as.data.frame(temp)
  nrml_music <- cbind(nrml_music, data.frame(temp))
}
nrml_music <- nrml_music[,-c(1)]

#standardisation with substraction of mean and division by standard deviation with scale function (as an alternative to normalization)
means = apply(music_cleaned, 2, mean)
sds = apply(music_cleaned, 2, sd)
nrml_music <- scale(music_cleaned, center = means, scale=sds)
nrml_music <- as.data.frame(nrml_music)  
#calculating the distance matrix
distance = dist(nrml_music)



kc <- kmeans(nrml_music,4)

#center of each cluster
kc$centers
#distribution of terms(genre in clusters)
table(music_cleaned$tempo, kc$cluster)

plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=kc$cluster)
plot(music_cleaned[c("artist.hotttnesss", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("bars_start", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("beats_start", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("duration", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("end_of_fade_in", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("loudness", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("start_of_fade_out", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("tatums_start", "song.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("song.hotttnesss", "tempo")], col=kc$cluster)
plot(music_cleaned[c("tempo", "song.hotttnesss")], col=kc$cluster)
#plot(music_cleaned[c("time_signature", "song.hotttnesss")], col=kc$cluster)

