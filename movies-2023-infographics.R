# Movie films infographics - 2023
#
# Author: Francesco Grassi
# Date: 01-05-23

# Create a little infographics with data about the movies I watched in 2023.
# Since I'm first codind this in May and will hopefully keep watching movies in the next months,
# the idea is to make the code as flexible as possible so to automatically update it when new movies
# will be added to the list.

# Huge HT to Tobias Stalder (https://tobias-stalder.netlify.app/dataviz/) for the inspiration coming from
# his amazing visualizations and code!

# The infographics will contain:
# - A circular bar plot, where the circle represents the months of the year. Each movie is a bar on the circle, 
#   and the lenght of the bar represents the duration of the movie

# Libraries --------
library(tidyverse)
library(lubridate)
library(geomtextpath)  # to rotate axis tick labels around polar chart
library(ggimage)  # to add platform labels to plots
library(cowplot)

# Load and clean data --------

# Data about movies is stored in "movies-2023.csv". File contains one row per movie, with columns:
# - Title
# - Date (of watching)
# - Platform
# - Duration (in min)
# - Genre
# - Rating (on a 1-to-5 scale)

df <- read_csv(file = "movies-2023.csv")  # Load raw data

# Clean df

# Turn "Date" into date format, and then split it into a "day" and "month" column
# (don't know if this is the right way, but still learning to handle dates in R)
# Also, convert "Platform" and "Genre" to factors.
# NOTE: must return on this point, since new genres might be added in the future
# and this approach is not very general so far (possibly same for "Platform".

df <- df %>% 
  mutate(Date = paste0(Date, "/23")) %>%  # Add year to date
  mutate(Date = dmy(Date)) %>%  # Turn it into date format
  mutate(Day = day(Date),  # Split Date into day and month
         Month = month(Date, label = TRUE, abbr = TRUE)) %>% 
  mutate(Platform = str_replace(Platform, "Prime", "Prime Video"),
         Platform = str_replace(Platform, "Cinema", "Theater")) %>% 
  mutate(Platform = factor(Platform),
         Genre = factor(Genre),
         Rating = factor(Rating, levels = seq(1, 5)))  # factorize Rating

# Relevant info data --------

# Get values from df that might turn useful in plotting:
n_movies <- nrow(df)  # number of movies
duration_min <- min(df$Duration)  # shortest movie duration
duration_max <- max(df$Duration)  # longest movie duration
duration_total <- sum(df$Duration)  # total minutes watched
duration_mean <- round(mean(df$Duration), 2)  # average movie length

# Summarize data --------

# Count number of movies and minutes watched per platform
platform_df <- df %>% 
  group_by(Platform) %>% 
  summarize(movie_num = n(),
            total_min = sum(Duration))

# Count number of movies and minutes watched per genre
genre_df <- df %>% 
  group_by(Genre) %>% 
  summarize(movie_num = n(),
            total_min = sum(Duration))

# Count number of movies per rating
rating_df <- df %>% 
  group_by(Rating, .drop = FALSE) %>%  # keep also empty ratings
  summarize(movie_num = n())

# Palettes --------

# Main color for plot elements:
plot_color <- "#D28389"

# Define a color palette for movie genres
# (this is just based on colors that I associate to my favorite movies of the genre)
col_genre <- c("#4F7389",  # Action ("The Bourne Identity")
               "#F7DB49",  # Animation ("Toy Story")
               "#6f0000",  # Crime ("Fargo")
               "#CB8F31",  # Drama ("No Country for Old Men")
               "#753081",  # Horror (Dario Argento's "Suspiria")
               "#C0CBCA",  # SciFi ("2001 A Space Odyssey")
               "#778F84",  # Thriller ("Memento")
               "#7B452A")  # War ("Full Metal Jacket")
# Give names to elements in the vector to reorder them later in plotting:
names(col_genre) <- c("Action", "Animation", "Crime", "Drama", "Horror", "SciFi", "Thriller", "War")

# General plot settings --------

plot_font <- "Baskerville"
plot_title_size <- 24
plot_subtitle_size <- 12
axis_title_size <- 16
axis_text_size <- 12
plot_title_color <- "#C06C84" # "#D28389"
axis_text_color <- "gray12"
plot_grid_color <- "#EAEAEA"

# Plotting --------

# 1. Movies per date, genre and duration

# A Plot showing months on a circle, with individual movies as radial bars.
# Length of the bar represents duration of the movie, while color represents genre
# NOTE: this plots seems to require a lot of tricks. I'm not sure if any of this is
# the easiest way to go for, but it's a first attempt.

date_plot <- df %>% 
  ggplot(aes(x = Date, y = Duration, fill = Genre)) +
  # Add custom panel y-grid (every 30 minutes)
  geom_segment(
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    data = data.frame(x1 = as.Date("2023-01-01"),  # starting point/date of the line
                      x2 = as.Date("2023-12-31"),  # ending point/date of the line
                      y = seq(0, 
                              plyr::round_any(duration_max, 60), # use "round_any" to find the nearest highest multiple of 30 mins 
                              by = 60)
    ),  
    color = plot_grid_color
    ) +
  # Add custom panel x-grid (one line per month)
  geom_segment(
    aes(x = x, xend = x, y = y1, yend = y2),
    inherit.aes = FALSE,
    data = data.frame(x = seq(ymd("2023-01-01"), ymd("2023-12-01"), by = "1 month"),  # lines at the beginning of each month
                      y1 = 0,
                      y2 = plyr::round_any(duration_max, 60) + 15  # slightly higher than longest movie duration
    ),
    color = plot_grid_color
  ) +
  # Add bars
  geom_col(width = 1, position = position_dodge2()) +
  # Add custom y-axis labels (movie duration)
  geom_text(
    aes(x = x, y = y, label = labels),
    inherit.aes = FALSE,
    data = data.frame(x = as.Date("2022-12-20"),  # start slightly before first month line
                      y = seq(60,  # skip the 0 min 
                              plyr::round_any(duration_max, 60),
                              by = 60),
                      labels = paste(seq(60, plyr::round_any(duration_max, 60), by = 60), "'")
    ),
    size = axis_text_size/.pt,  # must divide to .pt to scale it with rest of the text
    family = plot_font,
    color = axis_text_color
  ) +
  # Scale the x-axis
  # Set the limits slightly outside the year range so to leave a slice out (for duration text)
  scale_x_date(limits = as.Date(c("2022-12-20", "2024-01-10")), 
               breaks = seq(ymd("2023-01-01"), ymd("2023-12-01"), by = "1 month"),
               date_labels = "%b", 
               expand = c(0,0)
               ) +
  expand_limits(y = -30) +  # scale y axis so bars don't start from the center
  scale_fill_manual(values = col_genre # custom genre palette
                    # guide = guide_legend(  # move title of the legend on top (legend goes on bottom in theme())
                    #   nrow = 1,
                    #   title.position = "top",
                    #   title.hjust = 0.5)
                    ) +  
  labs(title = "Movies over the Year",
       subtitle = paste("Each bar is a movie watched during the year.",
                         "The lenght of the bar represents the duration of the movie,",
                         "the color of the bar indicates the genre.",
                         sep = "\n")) +
  coord_curvedpolar() +  # curve axes
  theme_void() +
  theme(
    text = element_text(color = axis_text_color, family = plot_font),
    panel.border = element_blank(),
    plot.margin = margin(30, 0, 15, 0, unit = "pt"),
    plot.title = element_text(size = plot_title_size, color = plot_title_color, margin = margin(10, 0, 5, 0, unit = "pt")),
    plot.subtitle = element_text(size = axis_title_size),
    axis.title = element_blank(),
    axis.text.x = element_text(size = axis_text_size, vjust = 1),  # get month labels closer to plot
    legend.position = "right",
    legend.background = element_blank(),
    legend.title = element_text(size = axis_title_size),
    legend.text = element_text(size = axis_text_size)
  )

date_plot

# 2. Minutes watched per platform

# Add logos filepath to df:
platform_df <- platform_df %>% 
  mutate(img_path = list.files(path = "assets", pattern = ".png", full.names = TRUE))

# Reorder platforms by total minutes
# NOTE: here uses ascending order. In the plot, highest values will be on top
platform_df <- platform_df %>% 
  mutate(Platform = fct_reorder(Platform, total_min))

# Create vector with platform logos to use as labels:
img_dir <- "assets/"  # dir containing logos
img_files <- list.files(path = img_dir, pattern = ".png")  # get file names
img_files <- img_files[order(match(img_files, paste0(levels(platform_df$Platform), ".png")))]  # reorder based on total minutes
# Now create HTML labels:
img_labels <- img_files %>% map_chr(\(filename)  # get each file name...
                                    paste0("<img src='",  # ...add HTML code before...
                                           img_dir,
                                           filename,
                                           "' width='50' />"))

# An horizontal bar plot showing number of minutes watched per platform.
platform_time_plot <- platform_df %>% 
  ggplot(aes(x = total_min, y = Platform)) + #, fill = Platform)) +
  geom_col(width = 0.7, fill = plot_title_color) +  # add bars
  geom_image(aes(x = 55, image = img_path, by = "height"), size = 0.07, asp = 1.8) +  # add logos inside bars
  scale_x_continuous(breaks = seq(0, plyr::round_any(max(platform_df$total_min), 100), by = 300), expand = c(0, 20)) +
  scale_y_discrete(expand = c(0, 0.3)) +
  labs(title = "Watching Time by Platform",
       subtitle = "Minutes of watching time on the different\nstreaming platforms and at the theater.",
       x = "Minutes") +
  theme_minimal() +
  theme(
    text = element_text(color = axis_text_color, family = plot_font),
    plot.margin = margin(10, 30, 10, 10, unit = "pt"),
    plot.title = element_text(size = plot_title_size, color = plot_title_color, margin = margin(10, 0, 10, 0, unit = "pt")),
    plot.subtitle = element_text(size = axis_title_size, margin = margin(0, 0, 25, 0, unit = "pt")),
    panel.grid.major.x = element_line(color = plot_grid_color),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = axis_title_size, margin = margin(10, 0, 0, 0, unit = "pt")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = axis_text_size, color = axis_text_color),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

platform_time_plot

# 3. Minutes watched per genre

# Reorder genre by total minutes
# NOTE: here uses ascending order. In the plot, highest values will be on top
genre_df <- genre_df %>% 
  mutate(Genre = fct_reorder(Genre, total_min))

# Reorder genre colors by total minutes:
col_genre <- col_genre[order(factor(names(col_genre), levels = levels(genre_df$Genre)))]

# An horizontal bar plot showing number of minutes watched per platform.
genre_time_plot <- genre_df %>% 
  ggplot(aes(x = total_min, y = Genre, fill = Genre)) +
  geom_col(width = 0.7) +  # add bars
  scale_x_continuous(breaks = seq(0, plyr::round_any(max(genre_df$total_min), 100), by = 200), expand = c(0, 20)) +
  scale_y_discrete(expand = c(0, 0.3)) +
  scale_fill_manual(values = col_genre) +
  labs(title = "Watching Time by Genre",
       subtitle = "Minutes of watching time for\ndifferent genres of movies.",
       x = "Minutes") +
  theme_minimal() +
  theme(
    text = element_text(color = axis_text_color, family = plot_font),
    plot.margin = margin(10, 20, 10, 10, unit = "pt"),
    plot.title = element_text(size = plot_title_size, color = plot_title_color, margin = margin(10, 0, 10, 0, unit = "pt")),
    plot.subtitle = element_text(size = axis_title_size, margin = margin(0, 0, 25, 0, unit = "pt")),
    panel.grid.major.x = element_line(color = plot_grid_color),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = axis_title_size, margin = margin(10, 0, 0, 0, unit = "pt")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = axis_text_size, color = axis_text_color),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

genre_time_plot

# 4. Movie rating

# A vertical bar plot showing number of movies per rating
rating_plot <- rating_df %>% 
  ggplot(aes(x = Rating, y = movie_num)) + # , fill = Rating)) +
  geom_col(width = 0.7, fill = plot_title_color) +  # add bars
  scale_x_discrete(expand = c(0, 0.2)) +
  scale_y_continuous(breaks = seq(0, max(rating_df$movie_num), by = 4), expand = c(0, 0.3)) +
  labs(title = "Movie Ratings",
       subtitle = "Number of movies by personal rating\n(on a 1-to-5 scale).",
       x = "Rating", 
       y = "Number of Movies") +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 20, 10, 10, unit = "pt"),
    text = element_text(color = axis_text_color, family = plot_font),
    plot.title = element_text(size = plot_title_size, color = plot_title_color, margin = margin(10, 0, 10, 0, unit = "pt")),
    plot.subtitle = element_text(size = axis_title_size, margin = margin(0, 0, 25, 0, unit = "pt")),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = plot_grid_color),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = axis_title_size, margin = margin(10, 0, 0, 0, unit = "pt")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = axis_text_size, color = axis_text_color),
    axis.text.x = element_text(size = axis_text_size, margin = margin(5, 0, 0, 0, unit = "pt")),
    axis.text.y = element_text(size = axis_text_size, margin = margin(0, 5, 0, 0, unit = "pt")),
    legend.position = "none"
  )

rating_plot

# Overall title and additional info --------

# Create text to display additional info about movies in the infographics
info_text <- paste(
  paste("In 2023 I watched", n_movies, "movies,"),
  paste("for a total watching time of", duration_total, "minutes, and a mean of", duration_mean, "minutes."),
  paste("The longest movie was", df %>% filter(Duration==max(Duration)) %>% pull(Title), ", with a duration of", duration_max, "minutes."),
  paste("The month of most watching was", 
        df %>% mutate(Month = month(Date, label=TRUE, abbr = FALSE)) %>% group_by(Month) %>% summarize(n=n()) %>% filter(n == max(n)) %>% pull(Month), 
        ", with",
        df %>% group_by(Month) %>% summarize(n=n()) %>% filter(n == max(n)) %>% pull(n),
        "movies."),
  sep = "\n")

# Create empty plot to display overall title and additional movie info (as subtitle)
title_plot <- ggplot() + 
  labs(title = "My 2023 in Movies", subtitle = info_text) + 
  theme_void() +
  theme(
    text = element_text(color = axis_text_color, family = plot_font),
    plot.title = element_text(size = 42, color = plot_title_color, hjust = 0.5, margin = margin(10, 0, 15, 0, unit = "pt")),
    plot.subtitle = element_text(size = axis_title_size, hjust = 0.5, margin = margin(0, 0, 0, 0, unit = "pt"))
  )

title_plot

# Create another empty plot to display personal info at the bottom of the figure
info_plot <- 
  # data.frame(x = 0, y = 0)
  ggplot() +
  annotate("text", x = 0, y = 0, label = "Francesco Grassi. GitHub: Fra-Grassi", size = 10/.pt, hjust = -0.6) +
  theme_void() +
  theme(
    text = element_text(color = axis_text_color, family = plot_font)
  )

info_plot
  
# Combining plots and save --------

movie_plot <- plot_grid(
  title_plot,
  date_plot,
  genre_time_plot,
  plot_grid(
    platform_time_plot,
    rating_plot,
    nrow = 1,
    rel_widths = c(1, 1)
  ),
  info_plot,
  nrow = 5,
  rel_heights = c(0.5, 2, 1, 1, 0.1)
)

ggsave(filename = "movies-2023.png", movie_plot, width = 2350, height = 5500, units = "px", dpi = 300, bg = "white")
