music <- read.csv("music.csv", header=T)
music_cleaned <- music[,c(1, 7, 9, 10, 11, 18, 24,26, 28, 29, 30, 32)]
dim(music_cleaned)
#removing the empty values
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
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

