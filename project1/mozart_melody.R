library(tidyverse)
library(data.table)

# Specify the folder path and pattern for TSV files in the "melodies" folder
mozart_files <- list.files(path = "mozart/notes", pattern = "*.tsv", full.names = TRUE)

# Read files and add a filename column called "song"
mozart_data <- lapply(mozart_files, function(x) {
  df <- read_tsv(file = x)
  df$song <- basename(x)  # Use basename to include just the filename
  return(df)
})

# Combine all data into one DataFrame
mozart_df <- rbindlist(mozart_data, fill = TRUE)

# Check distinct melodies based on selected columns
mozart_df |> distinct(song, name, octave, timesig)

# Filter and analyze pitches and their onsets
mozart_df |> 
  select(song, name, octave, timesig, mc_onset, mn_onset) |> 
  group_by(song, name, octave, timesig) |> 
  summarize(mc_onset_count = n_distinct(mc_onset), mn_onset_count = n_distinct(mn_onset), 
            .groups = "drop")

# Melodic bigram analysis function focusing on mc_onset
mozart_melodic_bigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> 
      select(mc_onset, mn_onset, name, octave) |> 
      mutate(next_mc_onset = lead(mc_onset), next_mn_onset = lead(mn_onset)) |> 
      count(mc_onset, next_mc_onset)
  } else {
    df_count <- data |> 
      select(mc_onset, mn_onset, name, octave) |> 
      mutate(next_mc_onset = lead(mc_onset), next_mn_onset = lead(mn_onset)) |> 
      count(mc_onset, next_mc_onset) |> 
      group_by(mc_onset) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}

mozart_bigram_data <- mozart_melodic_bigrams(data=mozart_df, percentage=TRUE) |> 
  arrange(desc(n))

# Melodic trigram analysis function
mozart_melodic_trigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> 
      select(mc_onset, mn_onset, name, octave) |> 
      mutate(next_mc_onset = lead(mc_onset), next_mn_onset = lead(mn_onset),
             next_next_mc_onset = lead(next_mc_onset)) |> 
      count(mc_onset, next_mc_onset, next_next_mc_onset)
  } else {
    df_count <- data |> 
      select(mc_onset, mn_onset, name, octave) |> 
      mutate(next_mc_onset = lead(mc_onset), next_mn_onset = lead(mn_onset),
             next_next_mc_onset = lead(next_mc_onset)) |> 
      count(mc_onset, next_mc_onset, next_next_mc_onset) |> 
      group_by(mc_onset) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}
mozart_trigram_data <- mozart_melodic_trigrams(data=mozart_df, percentage=TRUE)

# New melodic bigram analysis function focusing on 'name'
mozart_name_bigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name)) |> 
      count(name, next_name)
  } else {
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name)) |> 
      count(name, next_name) |> 
      group_by(name) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}

# Run the new function
mozart_name_bigram_data <- mozart_name_bigrams(data=mozart_df, percentage=TRUE) |> 
  arrange(desc(n))

# New melodic trigram analysis function focusing on 'name'
mozart_name_trigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name),
             next_next_name = lead(next_name)) |> 
      count(name, next_name, next_next_name)
  } else {
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name),
             next_next_name = lead(next_name)) |> 
      count(name, next_name, next_next_name) |> 
      group_by(name) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}

mozart_name_fivegrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name),
             next_next_name = lead(next_name),
             fourth_name = lead(next_next_name),
             fifth_name = lead(fourth_name)) |> 
      count(name, next_name, next_next_name, fourth_name, fifth_name)
  } else {
    df_count <- data |> 
      select(name, octave) |> 
      mutate(next_name = lead(name),
             next_next_name = lead(next_name),
             fourth_name = lead(next_next_name),
             fifth_name = lead(fourth_name)) |> 
      count(name, next_name, next_next_name, fourth_name, fifth_name)|> 
      group_by(name) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}

mozart_name_fivegram_data <- mozart_name_fivegrams(data=mozart_df, percentage=TRUE) |> 
  arrange(desc(n))

# Run the new function
mozart_name_trigram_data <- mozart_name_trigrams(data=mozart_df, percentage=TRUE) |> 
  arrange(desc(n))

mozart_name_bigram_data |> 
  filter(n > 200)

mozart_name_trigram_data |> 
  filter(n > 50)

mozart_name_fivegram_data


