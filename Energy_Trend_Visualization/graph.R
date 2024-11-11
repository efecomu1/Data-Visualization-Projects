###### Article link: https://www.nytimes.com/interactive/2023/11/20/climate/global-power-electricity-fossil-fuels-coal.html

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Load and preprocess data
df <- read_csv("yearly_full_release_long_format.csv")

generation_df <- df %>%
  filter(Category == "Electricity generation", Unit == "TWh") %>%
  select(Year, Area, Variable, Value)

# Exclude aggregated values like "Clean," "Fossil," and "Total Generation"
generation_df <- generation_df %>%
  filter(!Variable %in% c("Clean", "Fossil", "Total Generation"))

# Group variables and add PowerType for Clean vs. Fossil coloring later on
generation_df <- generation_df %>%
  mutate(
    Group = case_when(
      Variable == "Coal" ~ "Coal",
      Variable == "Gas" ~ "Gas",
      Variable == "Nuclear" ~ "Nuclear",
      Variable %in% c("Hydro", "Bioenergy", "Other Renewables") ~ "Hydroelectric and other",
      Variable %in% c("Wind", "Solar", "Wind and Solar") ~ "Wind and solar",
      Variable %in% c("Other Fossil", "Gas and Other Fossil", "Oil and other") ~ "Oil and other",
      TRUE ~ "Other"
    ),
    PowerType = case_when(
      Group %in% c("Coal", "Gas", "Oil and other") ~ "Fossil",
      Group %in% c("Nuclear", "Hydroelectric and other", "Wind and solar") ~ "Clean",
      TRUE ~ "Other"
    )
  )

# Filter for United States data
us_data <- generation_df %>%
  filter(Area == "United States of America", Group != "Other") %>%
  group_by(Year, Group, PowerType) %>%
  summarise(Total = sum(Value, na.rm = TRUE)) %>%
  mutate(Total_millions = Total / 1e3)

# Color schema for different Power Types
fossil_colors <- c("Coal" = "#FF8C00", "Gas" = "#B22222", "Oil and other" = "#FF4500")
clean_colors <- c("Nuclear" = "#2E8B57", "Hydroelectric and other" = "#66CDAA", "Wind and solar" = "#ADFF2F")

# Create the line chart for the United States
us_line_plot <- ggplot(us_data, aes(x = Year, y = Total_millions, color = Group)) +
  geom_line(size = 1.2) +
  labs(
    title = "United States Electricity Generation by Source",
    x = "Year",
    y = "Total Generation (Million TWh)",
    color = "Energy Source"
  ) +
  scale_color_manual(
    values = c(clean_colors, fossil_colors)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  ) +
  scale_y_continuous(labels = comma_format(scale = 1, suffix = "M"))

# Display the plot
print(us_line_plot)
