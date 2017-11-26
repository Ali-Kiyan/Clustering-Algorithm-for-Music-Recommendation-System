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
plot(music_cleaned$song.hotttnesss~ music_cleaned$artist.hotttnesss, data= music_cleaned)
#adding the terms(genre) --a lot of calculation
with(music_cleaned,text(music_cleaned$song.hotttnesss~ music_cleaned$artist.hotttnesss, labels=music_cleaned$terms,pos=4,cex=.6))
#removing the nominal value
music_cleaned <- music_cleaned[,-c(11)]

#getting the cols of the dataset

artist_hotness <- music_cleaned$artist.hotttness
bars_start <- music_cleaned$bars_start
beats_start <- music_cleaned$beats_start
duration <- music_cleaned$duration
start_of_fade_out <- music_cleaned$start_of_fade_out
end_of_fade_out <- music_cleaned$end_of_fade_in
loudness <- music_cleaned$loudness
song_hotness <- music_cleaned$song.hotttnesss
tatums_start <- music_cleaned$tatums_start
tempo <- music_cleaned$tempo
time_signature <- music_cleaned$time_signature
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
  nrml_music <- cbind(x, data.frame(temp))
}
nrml_music <- nrml_music[,-c(1)]

dim(music_cleaned)
newmin = -1
newmax= 1
oldmin <- min(tempo)
oldmax <- max(tempo)
newtempo <- ((tempo-oldmin)/(oldmax-oldmin))*(newmax-newmin) + newmin
newtempo <- as.data.frame(newtempo)
newtempo <- cbind(newtempo,data.frame(hotness))
newtempo <- newtempo[complete.cases(newtempo),]
nrml_music <- newtempo
means = apply(music_cleaned, 2, mean)
sds = apply(music_cleaned, 2, sd)
nrml_music <- scale(music_cleaned, center = means, scale=sds)
nrml_music <- as.data.frame(nrml_music)  
distance = dist(nrml_music)
distance
kc <- kmeans(nrml_music,4)
kc
kc$centers
table(music_cleaned$song.hotttnesss, kc$cluster)
plot(music_cleaned$tempo ~ music_cleaned$song.hotttnesss, data = music_cleaned,col=kc$cluster)
plot(music_cleaned[c("song.hotttnesss", "artist.hotttnesss")], col=kc$cluster)
plot(music_cleaned[c("song.hotttnesss", "tempo")], col=nrml_music$song.hotttnesss)
plot(music_cleaned[c("song.hotttnesss", "tempo")], col=nrml_music$end_of_fade_in)
