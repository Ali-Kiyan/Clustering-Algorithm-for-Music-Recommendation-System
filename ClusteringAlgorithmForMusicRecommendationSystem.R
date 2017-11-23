music <- read.csv("music.csv", header=T)
music_cleaned <- music[,c(7, 9, 11, 18, 24,26, 28, 29, 30, 32)]
dim(music_cleaned)
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
dim(music_cleaned)
library(cluster)
head(music_cleaned)
pairs(music_cleaned)
plot(music_cleaned$song.hotttnesss, music_cleaned$loudness, data= music_cleaned)
with(music_cleaned,text(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, lables=music_cleaned$terms,pos=4))
music_cleaned <- music_cleaned[,-c(9)]
means = apply(music_cleaned, 2, mean)
sds = apply(music_cleaned, 2, sd)
nrml_music <- scale(music_cleaned, center = means, scale=sds)
nrml_music 
distance = dist(nrml_music)
distance
kc <- kmeans(nrml_music,10)
kc$centers 
plot(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, data = music_cleaned,col=kc$cluster)
