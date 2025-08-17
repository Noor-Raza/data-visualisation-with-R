# Load required libraries
library(ggplot2)
library(sf)
library(tigris)
library(dplyr)
library(gganimate)
library(gifski)  
library(rnaturalearth)  
library(patchwork)  
library(magick)  
library(lubridate)
library(ggimage)  # For tree images
library(tidyr)    # For replace_na()
library(forcats)
library(cowplot)
library(ggtext)
library(RColorBrewer)

# Load dataset
load('/Users/noor/Downloads/09_SF_TREES/09_sf_trees_dd.RData')
load('/Users/noor/Downloads/09_SF_TREES/09_sf_trees.RData')

options(tigris_use_cache = TRUE)

title_plot <- ggdraw() +
  draw_label("The Evolution of San Francisco's Trees", 
             fontface = "bold", size = 20, hjust = 0.5, y = 0.6) +
  draw_label("Visualizing San Francisco's Tree Expansion Over the Last 65 Years", 
             fontface = "italic", size = 14, hjust = 0.5, y = 0.45)

# Map graph
trees <- sf_trees %>%
  mutate(
    PlantDate = as.Date(PlantDate, format = "%m/%d/%Y %I:%M:%S %p"),
    Year = year(PlantDate)
  ) %>%
  filter(!is.na(Longitude) & !is.na(Latitude) & !is.na(Year)) %>%
  arrange(Year, PlantDate)

# Get San Francisco census tracts
sf_tracts <- tracts(state = "06", county = "075", year = 2022, cb = TRUE)

# Suppress the centroid warning explicitly
options(warn = -1)  # Temporarily suppress warnings

# Filter out Farallon Islands and get centroids
sf_tracts_filtered <- sf_tracts %>%
  filter(!grepl("^06075098", GEOID)) %>%
  st_make_valid()

tract_centroids <- st_centroid(sf_tracts_filtered)

# Create a data frame that will track cumulative trees for each centroid over time
cumulative_growth <- trees %>%
  # Convert to spatial points
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(sf_tracts_filtered)) %>%
  # Find nearest centroid for each tree
  mutate(
    GEOID = sf_tracts_filtered$GEOID[st_nearest_feature(geometry, tract_centroids)]
  ) %>%
  # Sort by year to ensure proper accumulation
  arrange(Year) %>%
  # Create running count for each centroid
  group_by(GEOID) %>%
  mutate(
    cumulative_count = row_number()
  ) %>%
  ungroup()

# Create a complete grid of years and centroids
year_range <- seq(min(trees$Year), max(trees$Year), by = 1)
centroid_geoids <- unique(cumulative_growth$GEOID)

complete_grid <- expand.grid(
  Year = year_range,
  GEOID = centroid_geoids
) %>%
  arrange(Year, GEOID)

# For each year-GEOID combination, get the cumulative count
final_data <- complete_grid %>%
  left_join(
    cumulative_growth %>%
      st_drop_geometry() %>%
      group_by(GEOID, Year) %>%
      summarise(
        count_at_year = max(cumulative_count),
        .groups = "drop"
      ),
    by = c("GEOID", "Year")
  ) %>%
  group_by(GEOID) %>%
  mutate(
    # Fill in counts for years with no new trees
    count_at_year = cumsum(dplyr::coalesce(count_at_year, 0))
  ) %>%
  ungroup()

# Join with centroids
animated_centroids <- tract_centroids %>%
  left_join(final_data, by = "GEOID")

# Calculate the aspect ratio for both plots to ensure they're the same size
main_bbox <- st_bbox(sf_tracts_filtered)
downtown_bbox <- c(xmin = -122.42, xmax = -122.38, ymin = 37.78, ymax = 37.81)

# Calculate aspect ratios
main_ratio <- diff(c(main_bbox["ymin"], main_bbox["ymax"])) / diff(c(main_bbox["xmin"], main_bbox["xmax"]))
downtown_ratio <- diff(c(downtown_bbox["ymin"], downtown_bbox["ymax"])) / diff(c(downtown_bbox["xmin"], downtown_bbox["xmax"]))

# Define color scale for tree density
tree_colors <- colorRampPalette(c("lightgreen", "forestgreen", "darkgreen"))(5)

# Create custom legend for tree density
density_legend <- ggplot() +
  geom_point(data = data.frame(x = 1, y = 1:4, size = 10), 
             aes(x = x, y = y, fill = y), shape = 21, size = 5) +
  scale_fill_gradientn(name = "Tree Density",
                       colors = tree_colors,
                       breaks = 1:4,
                       labels = c("Low", "Medium", "High", "Very High")) +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white", color = "gray"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(face = "bold"),
        legend.box.margin = margin(5, 5, 5, 5))

# Extract legend manually
if (!requireNamespace("cowplot", quietly = TRUE)) {
  install.packages("cowplot")
}
library(cowplot)

color_legend <- get_legend(density_legend)

# Save the color legend
png("color_legend.png", width = 200, height = 250, bg = "transparent")
grid::grid.draw(color_legend)
dev.off()

# Create main animated map with NO legends
p1 <- ggplot() +
  geom_sf(data = sf_tracts_filtered, fill = "lightgreen", color = "gray", alpha = 0.2) +
  geom_sf(data = animated_centroids, aes(size = count_at_year, fill = count_at_year), 
          shape = 21, alpha = 0.8) +
  scale_size_continuous(range = c(1, 15)) +
  scale_fill_gradientn(colors = tree_colors) +
  annotate("rect", xmin = -122.42, xmax = -122.38, ymin = 37.78, ymax = 37.81, 
           color = "red", fill = NA, size = 1.5, linetype = "dotted") +
  theme_minimal() +
  coord_sf(xlim = c(-122.52, -122.35), 
           ylim = c(37.70, 37.85),
           expand = FALSE) +
  labs(title = "<b>Tree Growth in San Francisco</b>",
       subtitle = "Year: {as.integer(frame_time)}") +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(size = 16),
        plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank(),
        legend.position = "none") +  # Remove all legends from p1
  transition_time(Year) +
  ease_aes('linear')

# Create downtown zoomed-in map with both legends stacked vertically
p2 <- ggplot() +
  geom_sf(data = sf_tracts_filtered, fill = "lightgreen", color = "gray", alpha = 0.2) +
  geom_sf(data = animated_centroids, aes(size = count_at_year, fill = count_at_year), 
          shape = 21, alpha = 0.8) +
  scale_size_continuous(name = "Number of Trees",
                        range = c(1, 15),
                        breaks = c(100, 500, 1000, 2000)) +
  scale_fill_gradientn(name = "Tree Density",
                       colors = tree_colors,
                       breaks = c(0, 5000, 10000, 15000),
                       labels = c("Low", "Medium", "High", "Very High")) +
  theme_minimal() +
  coord_sf(xlim = c(-122.42, -122.38), 
           ylim = c(37.78, 37.81),
           expand = FALSE) +
  labs(title = "<b>Downtown San Francisco</b>",
       subtitle = "Year: {as.integer(frame_time)}") +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(size = 16),
        legend.position = "right",   # Position legends on the right
        legend.box = "vertical",     # Stack legends vertically
        legend.title = element_text(face = "bold"),
        legend.margin = margin(0, 0, 5, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(0.8, "lines"),  # Make legend items a bit smaller
        plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  guides(
    fill = guide_colorbar(order = 1),        # Tree Density on top
    size = guide_legend(order = 2)           # Number of Trees below
  ) +
  transition_time(Year) +
  ease_aes('linear')

# Set number of frames for animation
n_frames <- length(year_range) * 2  # 2 frames per year for smoother animation

# Adjust p2 width to accommodate the legends
anim1 <- animate(p1,
                 nframes = n_frames,
                 fps = 10,
                 width = 500,
                 height = 450)

anim2 <- animate(p2,
                 nframes = n_frames,
                 fps = 10,
                 width = 600,  # Increased width to accommodate legends
                 height = 450)

# Save animations
anim_save("main_map.gif", anim1)
anim_save("downtown.gif", anim2)

# Combine using magick
library(magick)
main_gif <- image_read("main_map.gif")
downtown_gif <- image_read("downtown.gif")

# Ensure both GIFs have the same number of frames
n_frames_actual <- min(length(main_gif), length(downtown_gif))
main_gif <- main_gif[1:n_frames_actual]
downtown_gif <- downtown_gif[1:n_frames_actual]

# Create a separator line below the animations
separator_line <- ggplot() + 
  geom_hline(yintercept = 0, color = "black", size = 1) + 
  theme_void()  # Use a void theme to remove axes and labels

# Save separator line
ggsave("separator_line.png", separator_line, width = 11, height = 0.2, dpi = 150)

# Create a vertical separator line for the bottom charts
vertical_separator <- ggplot() + 
  geom_vline(xintercept = 0, color = "black", size = 0.5) + # Thinner line
  theme_void()

# Save vertical separator
ggsave("vertical_separator.png", vertical_separator, width = 0.2, height = 3, dpi = 150)

# Clean and preprocess the dataset for the species distribution chart (p4)
trees_cleaned <- sf_trees %>%
  filter(!is.na(qSpecies) & qSpecies != "" & !is.na(PlantDate)) %>%
  mutate(
    qSpecies = sub(".::\\s", "", qSpecies), 
    qSpecies = trimws(qSpecies),  # Remove extra spaces
    qSpecies = ifelse(qSpecies == "", NA, qSpecies),  # Convert blanks to NA
    PlantYear = as.numeric(format(as.Date(PlantDate, format = "%m/%d/%Y %I:%M:%S %p"), "%Y"))  # Extract year
  ) %>%
  filter(!is.na(qSpecies)) %>%
  filter(!grepl("tree", qSpecies, ignore.case = TRUE))  # Remove trees

# Updated: Removed the filter for "Tree(s)"

# Count number of trees planted per species per year
trees_by_year <- trees_cleaned %>%
  group_by(qSpecies, PlantYear) %>%
  summarise(count = n(), .groups = "drop")

# Compute total count per species
species_total_counts <- trees_by_year %>%
  group_by(qSpecies) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))  # Sort species from most to least planted

top_species <- species_total_counts %>%
  slice_head(n = 5) %>%
  pull(qSpecies)

# Filter dataset for top species
trees_filtered <- trees_cleaned %>%
  filter(qSpecies %in% top_species) %>%
  left_join(species_total_counts, by = "qSpecies")  # Merge total_count

# Define Custom Green Color Palette (DARK GREEN for Most Planted → LIGHT GREEN for Least)
green_palette <- brewer.pal(5, "Greens")  # Default order: light → dark
color_map <- setNames(rev(green_palette), top_species)  #Reverse colors so DARK = Most Trees

# Create species labels with line breaks for long names
species_labels <- top_species
species_labels <- gsub(" ", "\n", species_labels)  # Add line breaks

# Final Violin Plot with Corrected Order and matching style
p4 <- ggplot(trees_filtered, aes(y = fct_reorder(qSpecies, total_count), x = PlantYear, fill = fct_reorder(qSpecies, total_count))) +
  geom_violin(alpha = 0.7, color = "black", width = 1.2) +  # Adjust width to prevent overlap
  geom_text(data = species_total_counts %>% filter(qSpecies %in% top_species), 
            aes(y = qSpecies, x = max(trees_filtered$PlantYear) + 2, label = total_count), 
            hjust = -0.2, size = 3) +  # Smaller text size
  scale_fill_manual(name = "Tree Species", values = color_map, guide = guide_legend(reverse = FALSE)) +  # Keep correct legend order
  scale_x_continuous(limits = c(1960, max(trees_filtered$PlantYear) + 10)) +  # Violin bars start at 1960
  scale_y_discrete(expand = expansion(mult = 0.3), labels = function(x) gsub(" ", "\n", x)) +  # Line breaks in y-axis labels
  labs(title = "<b>Tree Species Distribution</b>",
       subtitle = "Top 5 Most Planted Species",
       y = NULL, x = "Year of Planting") +
  theme_minimal(base_size = 10) +  # Smaller base font size
  theme(
    panel.background = element_rect(fill = "aliceblue"),  # Match the maps' background
    axis.text.y = element_text(size = 8, face = "bold", lineheight = 0.8), 
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    plot.title = element_markdown(size = 14),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5)  # Smaller margins
  )

# Clean and preprocess the dataset for the bar chart
trees_clean <- sf_trees %>%
  mutate(PlantDate = as.Date(PlantDate, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(Year = year(PlantDate)) %>%
  filter(!is.na(Year)) %>%
  mutate(YearGroup = floor(Year / 5) * 5)  # Group into 5-year bins

# Count trees per 5-year interval (TOTAL, no location split)
yearly_counts <- trees_clean %>%
  count(YearGroup) %>%
  filter(YearGroup < 2020)  # Remove 2020

# Ensure all 5-year intervals appear (even if 0 trees)
year_intervals <- data.frame(YearGroup = seq(floor(min(yearly_counts$YearGroup) / 5) * 5, 
                                             floor(max(yearly_counts$YearGroup) / 5) * 5, 
                                             by = 5))

# Join with actual data, filling missing intervals with 0
yearly_counts <- year_intervals %>%
  left_join(yearly_counts, by = "YearGroup") %>%
  mutate(n = replace_na(n, 0)) %>%  # Replace missing values with 0
  filter(YearGroup < 2020)  # Ensure 2020 is removed again

# Add tree icon URL using local file for optimizing performance
tree_icon_url <- "/Users/noor/Downloads/09_SF_TREES/5854298.png"

# Set a *constant* tree image size (same for all trees)
tree_size <- 0.2  # Adjust for better fit

# Calculate an appropriate Y-axis limit
y_max <- max(yearly_counts$n) * 1.2  # 20% padding to prevent cutoff

# Adjust tree positions
yearly_counts <- yearly_counts %>%
  mutate(TreeY = ifelse(n == 0, NA, n + (y_max * 0.03)))  # Hide trees if n == 0

# Increase tree position for specific years
yearly_counts <- yearly_counts %>%
  mutate(TreeY = ifelse(YearGroup == 1955, TreeY + (y_max * 0.07), TreeY)) %>%  # Extra lift for 1955
  mutate(TreeY = ifelse(YearGroup %in% c(1960, 1965, 1975), TreeY + (y_max * 0.05), TreeY))

# Adjust the bar chart with a matching background and smaller size
p3 <- ggplot() +
  # Tree trunks (bars)
  geom_segment(data = yearly_counts, aes(x = as.factor(YearGroup), xend = as.factor(YearGroup), 
                                         y = 0, yend = n), 
               color = "black", size = 3) +  # Reduced thickness
  geom_segment(data = yearly_counts, aes(x = as.factor(YearGroup), xend = as.factor(YearGroup), 
                                         y = 0, yend = n), 
               color = "#e07e5f", size = 2) +  # Reduced thickness
  # Trees at the top of bars
  geom_image(data = yearly_counts %>% filter(!is.na(TreeY)), 
             aes(x = as.factor(YearGroup), y = TreeY, image = tree_icon_url), size = 0.1) + # Smaller tree icons
  # Label for "No Tree" years
  geom_text(data = yearly_counts %>% filter(n == 0), 
            aes(x = as.factor(YearGroup), y = y_max * 0.05, label = "No Tree"), 
            color = "gray40", size = 2, fontface = "italic") +
  coord_cartesian(ylim = c(0, y_max)) +
  theme_minimal() +
  labs(
    title = "<b>Tree Growth Trend in 5-Year Intervals</b>",
    subtitle = "From 1955 to 2020",
    x = "Year",
    y = "Number of Trees",
    caption = NULL
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),  # Match the maps' background
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Smaller text
    legend.position = "none",
    plot.title = element_markdown(size = 14),             # Smaller title
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    axis.title = element_text(size = 9),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)                      # Smaller margins
  ) +
  # Add source caption at the beginning of x-axis
  annotate("text", x = 1, y = -y_max * 0.05, 
           label = "Source: San Francisco Tree Database (2025)", 
           hjust = 0, size = 2.5, fontface = "italic")

# Save both charts with matching dimensions
ggsave("tree_species_graph.png", p4, width = 5, height = 3, dpi = 150)
ggsave("tree_bar_graph.png", p3, width = 5, height = 3, dpi = 150)

# Save the title plot as an image
# First, determine the width of the combined plots
p1_width <- 500  # Width of main_map from anim1
p2_width <- 600  # Width of downtown from anim2
total_width <- p1_width + p2_width

# Save the title with the correct width
ggsave("title_image.png", title_plot, width = total_width / 100, 
       height = 1.5, units = "in", dpi = 150, bg = "white")

# Now let's combine everything with magick
library(magick)

# Read all components
main_gif <- image_read("main_map.gif")
downtown_gif <- image_read("downtown.gif")
separator <- image_read("separator_line.png")
vertical_sep <- image_read("vertical_separator.png")
bar_chart <- image_read("tree_bar_graph.png")
species_chart <- image_read("tree_species_graph.png")
title_img <- image_read("title_image.png")

# Get dimensions
n_frames_actual <- min(length(main_gif), length(downtown_gif))
main_gif <- main_gif[1:n_frames_actual]
downtown_gif <- downtown_gif[1:n_frames_actual]

# Get the width of the main_map (p1) and downtown (p2)
p1_width <- image_info(main_gif[1])$width
p2_width <- image_info(downtown_gif[1])$width
total_width <- p1_width + p2_width

# Resize the title to match the total width
title_img <- image_resize(title_img, paste0(total_width, "x", "!"))

# Resize charts to match widths
species_chart_resized <- image_resize(species_chart, paste0(p1_width, "x", "!"))
bar_chart_resized <- image_resize(bar_chart, paste0(p2_width, "x", "!"))

# Create the full separator line
separator_resized <- image_resize(separator, 
                                  paste0(total_width, "x", image_info(separator)$height))

# Build each frame
final_frames_with_title <- list()
for(i in 1:n_frames_actual) {
  # Combine maps horizontally
  top_row <- image_append(c(main_gif[i], downtown_gif[i]), stack = FALSE)
  
  # Combine bar chart and species chart horizontally
  bottom_row <- image_append(c(species_chart_resized, bar_chart_resized), stack = FALSE)
  
  # Stack everything vertically: maps, separator, then charts
  final_frame <- image_append(c(top_row, separator_resized, bottom_row), stack = TRUE)
  
  # Add title at the top
  final_frame_with_title <- image_append(c(title_img, final_frame), stack = TRUE)
  
  final_frames_with_title[[i]] <- final_frame_with_title
}

# Create the final animation
combined_gif_with_title <- image_join(final_frames_with_title)
combined_gif_with_title <- image_animate(combined_gif_with_title, fps = 10)

# Display and save
print(combined_gif_with_title)
image_write(combined_gif_with_title, "sf_trees_complete_visualization_with_title.gif")