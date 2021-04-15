# install.packages("spotifyr")
library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = "c22575ce003a46ba95e4590c10b3acb1")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "94cc674619fc44168db51564357111ad")

access_token <- get_spotify_access_token()

genres <- c("rap", "folk", "country", "rock", "blues", "jazz", "electronic", 
            "pop", "classical", "metal", "punk", "easy listening")

data <- NULL

for(i in 1:length(genres)){
  d <- get_genre_artists(genre = genres[i], limit = 50)
  d <- d[ , c("name", "genre")]
  data <- rbind(data, d)
  Sys.sleep(20)
}

write.csv(data, "artist_data.csv", row.names = FALSE)
