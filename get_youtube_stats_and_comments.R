# -- Hintergrund --

# Wir wollen die Metadaten und Kommentare der 1000 erfolgreichsten YouTube-Kanäle in Österreich ziehen.
# Hierfür greifen wir auf die Top 1000 Liste des Digitalreports. Es handelt sich um eine Liste basierend
# auf den Webseiten channelcrawler.com, youtuber-liste.at und der RTR Studie https://www.rtr.at/de/inf/YouTube_Channels_2017

# -- Setup --
library(readr)
library(stringr)
library(dplyr)

options(stringsAsFactors=FALSE) 

top_channels <- read_csv("https://raw.githubusercontent.com/Digitalreport/youtube/master/channels.csv")

# -- Statistiken von YouTube ziehen --

# Um die Daten von YouTube ziehen zu können, müssen wir bei Google ein Projekt registrieren und
# die API-Keys besorgen. Die von uns benötigte API ist die "YouTube Data API v3". Als Zugangsdaten
# benötigen wir die OAuth-Anmeldedaten (Typ: Sonstige). 
# https://console.developers.google.com/
# Die hier verwendeten API-Keys sind Dummies. Bitte die eigenen Daten verwenden.

# -- Setup --
library(tuber)

yt_oauth("123APICLIENT-ID.apps.googleusercontent.com",
         "123APICLIENTKEY",
         token = "")

# Da wir sehr viele Daten ziehen werden, sollten wir gebrauch von allen zur Verfügung stehenden Prozessoren
# machen. Dafür nutzen wir das Packet furrr - es ist die multiprocess-fähige Variante von purrr.
# Wir wandeln außerdem die Daten in einen Tibble um und erstellen mit lubridate ein passendes Format des Datums.

library(furrr)
library(purrr)
library(lubridate)
plan(multiprocess)

youtube_channel_stats <- future_map(as.character(top_channels$channelId),
                                    ~try(get_channel_stats(channel_id = .x)),
                                    .progress = TRUE)

youtube_channel_stats <- Filter(function(x) length(x) > 0, youtube_channel_stats)
# Anscheinend wurden drei Kanäle seitdem gelöscht.

youtube_channel_stats <- youtube_channel_stats %>%
  map(flatten) %>%
  {tibble(channelName = map_chr(., "title"),
          channelId = map_chr(., "id"),
          description = map_chr(., "description"),
          viewCount = map(., "viewCount") %>% as.integer(),
          subscriberCount = map(., "subscriberCount") %>% as.integer(),
          videoCount = map(., "videoCount") %>% as.integer(),
          publishedAt = map_chr(., "publishedAt") %>% as_datetime,
          thumbnailUrl = map(., "thumbnails") %>% map("medium") %>% map("url") %>% as.character()
  )}

# Nachdem wir die Statistiken der Kanäle gezogen haben, wollen wir jetzt die Statstiken der einzelnen Videos ziehen.
# Dafür holen wir uns alle Videos der Kanäle ab 2015.

youtube_videos <- future_map_dfr(as.character(youtube_channel_stats$channelId),
                                 ~yt_search(term="",
                                            type="video",
                                            channel_id = .x,
                                            published_after = "2015-01-01T00:00:00Z"),
                                 .progress = TRUE) %>%
  mutate(publishedAt = as_datetime(publishedAt)) %>%
  rename(videoId = video_id)

youtube_video_stats <- future_map_dfr(as.character(youtube_videos$videoId),
                                      ~get_stats(video_id = .x),
                                      .progress = TRUE) %>%
  dplyr::rename(videoId = id) %>%
  mutate_at(vars(dplyr::matches("Count")), funs(as.integer))


youtube_videos <- youtube_videos %>%
  left_join(youtube_video_stats, by = "videoId") %>%
  as_tibble()

rm(youtube_video_stats)

# Jetzt können wir auch noch die Kommentare zu den Videos ziehen. Nur die Videos mit mehr als 10 Kommentaren.

# youtube_comments <- future_map_dfr(youtube_videos %>%
#                                      filter(commentCount > 10) %>%
#                                      pull(videoId),
#                                    ~get_all_comments(video_id = .x),
#                                    .progress = TRUE)

# Ziemlich sicher würden wir damit unsere 1 Mio. Credits an API-Aufrufen verbrauchen. 
# Damit letzteres funktioniert, müssen wir das über mehrere Tage verteilt machen.

youtube_comments_1 <- future_map_dfr(youtube_videos[1:10000] %>%
                                       filter(commentCount > 10) %>%
                                       pull(videoId),
                                     ~get_all_comments(video_id = .x),
                                     .progress = TRUE)

youtube_comments_2 <- future_map_dfr(youtube_videos[10001:20000] %>%
                                       filter(commentCount > 10) %>%
                                       pull(videoId),
                                     ~get_all_comments(video_id = .x),
                                     .progress = TRUE)

# …
# Anschließend die Listen auf Fehler filtern und in einen Dataframe umwandeln

youtube_comments_1 <- Filter(function(x) class(x) != "try-error", youtube_comments_1) %>%
  rbindlist() %>%
  as_tibble()

# …

youtube_comments <- rbind_all(youtube_comments_1,
                              youtube_comments_2)

