music <- read.csv("music.csv", header=T)
music_cleaned <- music[,c(7, 9, 11, 18, 24,26, 28, 29, 30, 32)]
dim(music_cleaned)
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
dim(music_cleaned)
library(cluster)
head(music_cleaned)
pairs(music_cleaned)
plot(music_cleaned$song.hotttnesss, music_cleaned$loudness, data= music_cleaned)
with(music_cleaned,text(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, lables=music_cleaned$time_signature))
