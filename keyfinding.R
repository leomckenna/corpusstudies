#remotes::install_github('jaburgoyne/compmus')
#devtools::install_github('charlie86/spotifyr')

library(compmus)
library(spotifyr)
library(tidyverse)

client_id <- "12317ca7cb97425fb601a57d9f7d6d13"
client_secret <- "9be5412331974075902e90a3ba91fb77"

# Retrieve the access token
access_token <- get_spotify_access_token(
  client_id = client_id,
  client_secret = client_secret
)

df <- get_playlist_audio_features("", "74MfBIVOVHzQiJqfJ1dgTo") |> 
  add_audio_analysis()

df |> 
  mutate(segments = map2(segments, key, compmus_c_transpose)) |>
  select(segments) |> unnest(segments)


# Now proceed with getting the playlist audio features
funk_key_profile <- get_playlist_audio_features("", "3NJ9LIkDJukV5jXCvmUZxs") |>
  add_audio_analysis() |>
  mutate(segments = map2(segments, key, compmus_c_transpose)) |>
  select(segments) |>
  unnest(segments) |> 
  select(start, duration, pitches) |> 
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  group_by(pitch_class) |>
  summarise(mean_value = mean(value))

funk_key_profile

get_key_profile_broad <- function(uri){
  get_playlist_audio_features("", uri) |>
    add_audio_analysis() |>
    ## transpose all the chroma vectors to C. (have I mentioned how great Burgoyne's library is??)
    mutate(segments = map2(segments, key, compmus_c_transpose)) |>
    ## grab the segments data and unnest it, then only grabbing the start, duration, and pitches info.
    select(segments) |>
    unnest(segments) |> 
    select(start, duration, pitches) |> 
    mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
    compmus_gather_chroma() |>
    group_by(pitch_class) |>
    summarise(mean_value = mean(value)) 
}

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}
# 
# # ### uses the Krumhansl Schmuckler Profiles
major_key <- c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

##sapp's simple weightings
# major_key <- c(2, 0, 1, 0, 1, 1, 0, 2, 0, 1, 0, 1)
# 
# minor_key <- c(2, 0, 1, 1, 0, 1, 0, 2, 1, 0, 0.5, 0.5)

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

i_wish <-
  get_tidy_audio_analysis("74MfBIVOVHzQiJqfJ1dgTo") |>
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

i_wish |> 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")


