# ==============================================================================
# India Heat Exposure and Disease Burden Analysis
# Emergency Public Health Data Project
# ==============================================================================

# Load required packages
library(tidyverse)
library(knitr)

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

# Load disease burden data (DALYs or mortality by state)
# Expected columns: state, dalys (or mortality_rate), year
disease_data <- read_csv("disease_burden.csv")

# Load heat exposure data (temperature or heatwave days by state)
# Expected columns: state, mean_max_temp (or heatwave_days), year
heat_data <- read_csv("heat_exposure.csv")

# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

# Merge datasets by state (and year if applicable)
# Adjust join keys based on your data structure
merged_data <- disease_data %>%
  inner_join(heat_data, by = c("state", "year"))

# Remove any rows with missing values
merged_data <- merged_data %>%
  filter(!is.na(dalys) & !is.na(mean_max_temp))

# ==============================================================================
# 3. SCATTER PLOT: Heat Exposure vs Disease Burden
# ==============================================================================

scatter_plot <- ggplot(merged_data, aes(x = mean_max_temp, y = dalys)) +
  geom_point(size = 3, alpha = 0.7, color = "#E74C3C") +
  geom_smooth(method = "lm", se = TRUE, color = "#3498DB", linetype = "dashed") +
  labs(
    title = "Heat Exposure vs Disease Burden Across Indian States",
    x = "Mean Maximum Temperature (°C)",
    y = "DALYs per 100,000 Population",
    caption = "Source: State-level health and climate data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave("scatter_heat_disease.png", scatter_plot, width = 8, height = 6, dpi = 300)
print(scatter_plot)

# ==============================================================================
# 4. BAR PLOT: Top 10 States by Disease Burden
# ==============================================================================

# Calculate average disease burden by state (if multiple years)
state_summary <- merged_data %>%
  group_by(state) %>%
  summarise(
    avg_dalys = mean(dalys, na.rm = TRUE),
    avg_temp = mean(mean_max_temp, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_dalys)) %>%
  slice(1:10)

bar_plot <- ggplot(state_summary, aes(x = reorder(state, avg_dalys), y = avg_dalys)) +
  geom_bar(stat = "identity", fill = "#E67E22", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 States by Disease Burden",
    x = "State",
    y = "Average DALYs per 100,000 Population",
    caption = "Ranked by average disease burden"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

# Save plot
ggsave("bar_top_states.png", bar_plot, width = 8, height = 6, dpi = 300)
print(bar_plot)

# ==============================================================================
# 5. SUMMARY TABLE: Descriptive Statistics by Region
# ==============================================================================

# Calculate summary statistics
summary_table <- merged_data %>%
  summarise(
    n_states = n_distinct(state),
    n_observations = n(),
    mean_dalys = mean(dalys, na.rm = TRUE),
    sd_dalys = sd(dalys, na.rm = TRUE),
    min_dalys = min(dalys, na.rm = TRUE),
    max_dalys = max(dalys, na.rm = TRUE),
    mean_temp = mean(mean_max_temp, na.rm = TRUE),
    sd_temp = sd(mean_max_temp, na.rm = TRUE),
    min_temp = min(mean_max_temp, na.rm = TRUE),
    max_temp = max(mean_max_temp, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Print formatted table
kable(summary_table, 
      caption = "Summary Statistics: Heat Exposure and Disease Burden",
      format = "markdown")

# Save table
write_csv(summary_table, "summary_statistics.csv")

# Additional table: State-level details
state_table <- state_summary %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  rename(
    State = state,
    `Avg DALYs` = avg_dalys,
    `Avg Temp (°C)` = avg_temp
  )

kable(state_table,
      caption = "State-Level Summary: Disease Burden and Heat Exposure",
      format = "markdown")

write_csv(state_table, "state_level_summary.csv")

# ==============================================================================
# 6. CORRELATION ANALYSIS
# ==============================================================================

# Calculate correlation coefficient
correlation <- cor(merged_data$mean_max_temp, merged_data$dalys, 
                   use = "complete.obs")

cat("\n==============================================================================\n")
cat("CORRELATION ANALYSIS\n")
cat("==============================================================================\n")
cat(sprintf("Pearson correlation coefficient: %.3f\n", correlation))
cat("Interpretation: ", 
    ifelse(abs(correlation) < 0.3, "Weak",
           ifelse(abs(correlation) < 0.7, "Moderate", "Strong")),
    ifelse(correlation > 0, " positive", " negative"),
    " correlation\n")
cat("==============================================================================\n")

# ==============================================================================
# END OF ANALYSIS
# ==============================================================================
