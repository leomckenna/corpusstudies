library(tidyverse)
library(data.table)
library(reshape2)

# Specify the folder path and pattern for TSV files in the "harmonies" folder
mozart_files <- list.files(path = "mozart/harmonies", pattern = "*.tsv", full.names = TRUE)

# Read files and add a filename column called "song"
mozart_data <- lapply(mozart_files, function(x) {
  df <- read_tsv(file = x)
  df$song <- basename(x)  # Use basename to include just the filename
  return(df)
})

# Combine all data into one DataFrame
mozart_df <- rbindlist(mozart_data, fill = TRUE)

# Perform distinct analysis
mozart_df |> distinct(song, globalkey, timesig)

# Filter and analyze cadences
mozart_df |> filter(grepl("C", cadence)) |> 
  select(song, cadence) |> 
  group_by(song) |> 
  count(cadence, sort = TRUE) |>
  mutate(percentage = n / sum(n) * 100)

# Harmonic bigram analysis function
mozart_harmonic_bigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral)) |> 
      count(numeral, next_numeral)
  } else {
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral)) |> 
      count(numeral, next_numeral) |> group_by(numeral) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}
mozart_bigram_data <- mozart_harmonic_bigrams(data=mozart_df, percentage=TRUE)

# Harmonic trigram analysis function
mozart_harmonic_trigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral),
             next_next_numeral = lead(next_numeral)) |> 
      count(numeral, next_numeral, next_next_numeral)
  } else {
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral),
             next_next_numeral = lead(next_numeral)) |> 
      count(numeral, next_numeral, next_next_numeral) |> group_by(numeral) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}
mozart_trigram_data <- mozart_harmonic_trigrams(data=mozart_df, percentage=TRUE)

# Matrix plotting function
matrix_plotter <- function(data_for_matrix, pct=FALSE){
  if(pct==FALSE){
    ggplot(data_for_matrix, aes(x = next_numeral, y = numeral, fill = n)) +
      geom_tile(color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = n), color = "white", size = 2) +
      coord_fixed() + theme_bw()
  } else {
    ggplot(data_for_matrix, aes(x = next_numeral, y = numeral, fill = percentage)) +
      geom_tile(color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = as.numeric(percentage)), color = "white", size = 2) +
      coord_fixed() + theme_bw()
  }
}

# Plotting the matrices for bigram and trigram data
matrix_plotter(mozart_bigram_data, pct=T)
matrix_plotter(mozart_bigram_data, pct=F)

library(tidyverse)
library(data.table)

# Specify the folder path and pattern for TSV files in the "harmonies" folder
beethoven_files <- list.files(path = "beethoven/harmonies", pattern = "*.tsv", full.names = TRUE)

# Read files and add a filename column called "song"
beethoven_data <- lapply(beethoven_files, function(x) {
  df <- read_tsv(file = x)
  df$song <- basename(x)  # Use basename to include just the filename
  return(df)
})

# Combine all data into one DataFrame
beethoven_df <- rbindlist(beethoven_data, fill = TRUE)

# Perform distinct analysis
beethoven_df |> distinct(song, globalkey, timesig)

# Filter and analyze cadences
beethoven_df |> filter(grepl("C", cadence)) |> 
  select(song, cadence) |> 
  group_by(song) |> 
  count(cadence, sort = TRUE) |>
  mutate(percentage = n / sum(n) * 100)

# Harmonic bigram analysis function
beethoven_harmonic_bigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral)) |> 
      count(numeral, next_numeral)
  } else {
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral)) |> 
      count(numeral, next_numeral) |> group_by(numeral) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}
beethoven_bigram_data <- beethoven_harmonic_bigrams(data=beethoven_df, percentage=TRUE)

# Harmonic trigram analysis function
beethoven_harmonic_trigrams <- function(data, percentage=FALSE){
  if(percentage == FALSE){
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral),
             next_next_numeral = lead(next_numeral)) |> 
      count(numeral, next_numeral, next_next_numeral)
  } else {
    df_count <- data |> filter(mn_onset == 0) |> select(numeral) |> 
      mutate(next_numeral = lead(numeral),
             next_next_numeral = lead(next_numeral)) |> 
      count(numeral, next_numeral, next_next_numeral) |> group_by(numeral) |> 
      mutate(percentage = round(n / sum(n) * 100, digits=2)) 
  }
  return(df_count)
}
beethoven_trigram_data <- beethoven_harmonic_trigrams(data=beethoven_df, percentage=TRUE)

# Matrix plotting function
matrix_plotter <- function(data_for_matrix, pct=FALSE){
  if(pct==FALSE){
    ggplot(data_for_matrix, aes(x = next_numeral, y = numeral, fill = n)) +
      geom_tile(color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = n), color = "white", size = 2) +
      coord_fixed() + theme_bw()
  } else {
    ggplot(data_for_matrix, aes(x = next_numeral, y = numeral, fill = percentage)) +
      geom_tile(color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = as.numeric(percentage)), color = "white", size = 2) +
      coord_fixed() + theme_bw()
  }
}

# Plotting the matrices for bigram and trigram data
matrix_plotter(beethoven_bigram_data, pct=T)
matrix_plotter(beethoven_bigram_data, pct=F)




# Summary for Beethoven bigrams
beethoven_bigram_summary <- beethoven_bigram_data |> 
  group_by(numeral, next_numeral) |> 
  summarize(total_count = sum(n), percentage = mean(percentage), .groups = 'drop')


# Summary for Mozart bigrams
mozart_bigram_summary <- mozart_bigram_data |> 
  group_by(numeral, next_numeral) |> 
  summarize(total_count = sum(n), percentage = mean(percentage), .groups = 'drop')

# Summary for Beethoven trigrams
beethoven_trigram_summary <- beethoven_trigram_data |> 
  group_by(numeral, next_numeral, next_next_numeral) |> 
  summarize(total_count = sum(n), percentage = mean(percentage))

# Summary for Mozart trigrams
mozart_trigram_summary <- mozart_trigram_data |> 
  group_by(numeral, next_numeral, next_next_numeral) |> 
  summarize(total_count = sum(n), percentage = mean(percentage))

# Display summaries
print(beethoven_bigram_summary)
print(mozart_bigram_summary)

print(beethoven_trigram_summary)
print(mozart_trigram_summary)

combined_data <- full_join(beethoven_bigram_summary, mozart_bigram_summary,
                           by = c("numeral", "next_numeral"),
                           suffix = c("_beethoven", "_mozart"))

# Fill missing values (NAs) with 0 in the total_count columns
combined_data <- combined_data %>%
  mutate(total_count_beethoven = replace_na(total_count_beethoven, 0),
         total_count_mozart = replace_na(total_count_mozart, 0))

# Create a contingency table with Beethoven and Mozart total counts
contingency_table <- combined_data %>%
  select(numeral, next_numeral, total_count_beethoven, total_count_mozart)

# Convert the data to a matrix format for the chi-squared test
contingency_matrix <- as.matrix(contingency_table %>%
                                  select(total_count_beethoven, total_count_mozart))

# Perform the chi-squared test
chi_squared_result <- chisq.test(contingency_matrix)

# Print the results
print(chi_squared_result)

