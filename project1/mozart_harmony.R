library(tidyverse)
library(data.table)

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

#matrix plot function
matrix_plotter <- function(data_for_matrix, pct=FALSE){
  base_plot <- ggplot(data_for_matrix, aes(x = next_numeral, y = numeral)) +
    coord_fixed() + 
    theme_minimal(base_size = 12) +  # Change theme to minimal
    theme(
      panel.background = element_rect(fill = "black"),  # Panel background color
      legend.position = "right",  # Position the legend to the right
      legend.title = element_text(face = "bold", size = 10),  # Bold legend title
      legend.text = element_text(size = 8),  # Legend text size
      axis.title.x = element_text(face = "bold"),  # Bold x-axis title
      axis.title.y = element_text(face = "bold"),  # Bold y-axis title
      panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    )
  
  if(pct==FALSE){
    base_plot +
      geom_tile(aes(fill = n), color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = n), color = "white", size = 2) +
      labs(fill = "Count")  # Add a legend title
  } else {
    base_plot +
      geom_tile(aes(fill = percentage), color = "white", lwd = 2, linetype = 1) +
      geom_text(aes(label = as.numeric(percentage)), color = "white", size = 2) +  # Format percentage
      labs(fill = "Percentage")  # Add a legend title
  }
}

mozart_t_plot <- matrix_plotter(mozart_bigram_data, pct=T)
mozart_f_plot <- matrix_plotter(mozart_bigram_data, pct=F)

ggsave("mozart_t_plot.png", plot = mozart_t_plot, width = 8, height = 6, dpi = 300)
ggsave("mozart_f_plot.png", plot = mozart_f_plot, width = 8, height = 6, dpi = 300)
