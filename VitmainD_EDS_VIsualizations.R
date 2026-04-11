# Vitamin D Deficiency and Quality of Life in EDS Patients - Visualizations

# 0 --- Load Libraries ---
install.packages("ggplot2")
install.packages("rlang")
library(ggplot2)
library(tidyr)
library(dplyr)
library(gt)


# 1 --- Summary Table ---

# 1.1. Recreate your summary data
table_data <- data.frame(
  Quality_of_Life = factor(
    c("Poor", "Fair", "Good", "Very Good-Excellent"), 
    levels = c("Poor", "Fair", "Good", "Very Good-Excellent")
  ),
  Deficiency = c(53, 160, 139, 94),
  No_Deficiency = c(74, 232, 265, 216)
)

# 1.2. Generate the custom formatted table
table_data %>%
  gt() %>%
  tab_header(
    title = NULL
  ) %>%
  cols_label(
    Quality_of_Life = "Quality of Life",
    Deficiency = "Vitamin D Deficiency",
    No_Deficiency = "No Vitamin D Deficiency"
  ) %>%
  cols_align(align = "center", columns = c(Deficiency, No_Deficiency)) %>%
  tab_options(
    heading.background.color = "white",
    column_labels.background.color = "#8C1D40",
    table.border.top.color = "#8C1D40",
    table.border.bottom.color = "#8C1D40",
    table_body.border.bottom.color = "#8C1D40",
    column_labels.border.bottom.color = "#8C1D40"
  ) %>%
  # --- Title text black ---
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  
  # --- Column labels white so they show on maroon ---
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # --- Add light maroon background to every other row for readability ---
  tab_style(
    style = cell_fill(color = "#f4e9ec"), 
    locations = cells_body(rows = seq(2, nrow(table_data), by = 2))
  )
#--------------END of Summary Table------------------










# 2 --- Grouped Bar Chart with Error Bars ---

# 2.1. Group the data counts
# Poor (53) + Fair (160) = 213
# Good (139) + Very Good-Excellent (94) = 233
deficiency_grouped <- c(213, 233) 

# Poor (74) + Fair (232) = 306
# Good (265) + Very Good-Excellent (216) = 481
no_def_grouped <- c(306, 481)

# Create the new grouped labels
qol_grouped_labels <- c("Poor/Fair", "Good/Excellent")

#2.2. Build the initial data frame
df_grouped <- data.frame(
  QoL_Group = factor(qol_grouped_labels, levels = qol_grouped_labels, ordered = TRUE),
  `Vitamin D Deficiency` = deficiency_grouped,
  `No Deficiency` = no_def_grouped,
  check.names = FALSE
)

# 2.3. Calculate proportions and standard errors for the 95% CI error bars
total_n <- sum(df_grouped$`Vitamin D Deficiency`) + sum(df_grouped$`No Deficiency`)

df_plot <- df_grouped %>%
  pivot_longer(cols = c(`Vitamin D Deficiency`, `No Deficiency`), 
               names_to = "VitaminD_Status", 
               values_to = "Count") %>%
  mutate(
    # Relative frequency (proportion, p)
    p = Count / total_n,
    
    # Standard error for a proportion: sqrt(p(1-p)/n)
    se = sqrt((p * (1 - p)) / total_n),
    
    # Calculate 95% Confidence Intervals (p +/- 1.96 * SE)
    ci_lower = p - (1.96 * se),
    ci_upper = p + (1.96 * se))

# 2.4. Create the Grouped Bar Chart with Error Bars
ggplot(df_plot, aes(x = QoL_Group, y = p, fill = VitaminD_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           color = "black", width = 0.7, linewidth = 0.5) +
  
  # Add the error bars using the calculated confidence intervals
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.8), 
                width = 0.15, linewidth = 0.7) +
  
  # Apply Blue and Orange colors
  scale_fill_manual(values = c("Vitamin D Deficiency" = "#8C1D40",  # Orange
                               "No Deficiency" = "#FFC627")) +      # Blue
  
  # Format y-axis as percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Add percentage labels (shifted slightly above the top of the error bar)
  # *Note: bumped the offset from 0.015 to 0.02 since the bars are taller now
  geom_text(aes(label = scales::percent(p, accuracy = 0.1), y = ci_upper + 0.02), 
            position = position_dodge(width = 0.8), 
            vjust = 0, size = 4, fontface = "bold") +
  theme_minimal() +
  labs(
    title = NULL, # No main title 
    subtitle = NULL, # No subtitle
    x = "Quality of Life Category",
    y = "Relative Frequency (%)",
    fill = "Vitamin D Status"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)), # Adds space to the Top of the x-axis title
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 15)), # Adds space to the Right of the y-axis title
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12)
  )


# --- Grouped Bar Chart with Error Bars END ---













# 5.3 --- Mann-Whitney U Test Visualization: Jittered Point Plot with Median Ranks ---

# 5.3.1. Recreate the individual, row-level data for the rank test
qol_labels <- c("Poor", "Fair", "Good", "Very Good\nExcellent")
deficiency <- c(53, 160, 139, 94)
no_deficiency <- c(74, 232, 265, 216)

qol_factor <- factor(rep(qol_labels, times = deficiency + no_deficiency), 
                     levels = qol_labels, ordered = TRUE)

status_vector <- c(
  rep("Deficiency", deficiency[1]), rep("No Deficiency", no_deficiency[1]),
  rep("Deficiency", deficiency[2]), rep("No Deficiency", no_deficiency[2]),
  rep("Deficiency", deficiency[3]), rep("No Deficiency", no_deficiency[3]),
  rep("Deficiency", deficiency[4]), rep("No Deficiency", no_deficiency[4])
)

df_ranks <- data.frame(
  QoL = qol_factor, 
  VitaminD = factor(status_vector, levels = c("Deficiency", "No Deficiency")),
  # Convert the ordinal categories to numeric ranks (1 to 4) for plotting
  Rank = as.numeric(qol_factor) 
)

# 5.3.2. Build the Jittered Point Plot
ggplot(df_ranks, aes(x = VitaminD, y = Rank, color = VitaminD)) +
  
  # 5.3.2.A. The "Cloud": Jittered points representing individual patients
  # alpha = 0.3 makes them semi-transparent so dense areas appear darker
  geom_jitter(width = 0.25, height = 0.2, alpha = 0.3, size = 2.5) +
  
  # 5.3.2.B. The Median Marker: A bold crossbar showing the exact center of the ranks
  stat_summary(fun = median, geom = "crossbar", width = 0.5, 
               color = "gray50", linewidth = .5) +
  
  # Styling and Colors
  scale_color_manual(values = c("Deficiency" = "#8C1D40",  # Maroon
                                "No Deficiency" = "#FFC627")) +      # Gold
  
  # Map the numeric y-axis (1, 2, 3, 4) back to the text labels
  scale_y_continuous(breaks = 1:4, labels = qol_labels) +
  
  theme_minimal() +
  labs(
    title = NULL, # No main title 
    subtitle = NULL, # No subtitle
    x = "Vitamin D Status",
    y = "Quality of Life Rating"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 5), hjust = 1.5),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15), hjust = 1.0),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(face = "bold"), # Make the bottom labels pop
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 17, face = "bold"),
    legend.position = "none", # Legend is redundant since x-axis has the labels
    panel.grid.minor = element_blank() # Clean up background lines
  )

# --- Mann-Whitney U Test Visualization: Jittered Point Plot with Median Ranks END---

# 5.4 --- Bubble Trend Plot: Chi-squared Test for Trend in Proportion of Deficiency Across QoL Categories ---
##-- Cochran-Armitage Test for Trend ---

# 5.4.1. Recreate the data and calculate proportions
qol_labels <- c("Poor", "Fair", "Good", "Very Good \n Excellent")
df_trend <- data.frame(
  QoL = factor(qol_labels, levels = qol_labels, ordered = TRUE),
  Deficiency = c(53, 160, 139, 94),
  No_Deficiency = c(74, 232, 265, 216)
) %>%
  mutate(
    Total = Deficiency + No_Deficiency,
    Prop_Def = Deficiency / Total
  )

# 5.4.2. Build the Bubble Trend Plot
ggplot(df_trend, aes(x = QoL, y = Prop_Def, group = 1)) +
  
  # 5.4.2.A. The Trend Line (Blue)
  geom_line(color = "#FFC627", linewidth = 1.5) +
  
  # 5.4.2.B. The Bubbles (Orange): Size mapped to total patients in that QoL group
  geom_point(aes(size = Total), color = "#8C1D40", alpha = 0.9) +
  
  # 5.4.2.C. Inner white dot to give the bubbles a polished, "donut" look
  geom_point(size = 2, color = "white") +
  
  # 5.4.2.D. Percentage labels hovering just above the bubbles
  geom_text(aes(label = scales::percent(Prop_Def, accuracy = 1)), 
            vjust = -2.5, size = 4.5, fontface = "bold", color = "gray20") +
  
  # Scale adjustments
  scale_size_continuous(range = c(6, 18), guide = "none") + # Hide the size legend
  scale_y_continuous(labels = scales::percent_format(), 
                     limits = c(0.25, max(df_trend$Prop_Def) + 0.08)) +
  
  theme_minimal() +
  labs(
    title = NULL, # No main title 
    subtitle = NULL, # No subtitle
    x = "Quality of Life Category",
    y = "Proportion with Vitamin D Deficiency (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5), hjust = 1.20),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)), # Adds space to the Top of the x-axis title
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 15)), # Adds space to the Right of the y-axis title
    panel.grid.minor = element_blank(), # Clean up background lines
    panel.grid.major.x = element_blank() # Remove vertical grid lines for a cleaner look
  )
# --- Bubble Trend Plot: Chi-squared Test for Trend in Proportion of Deficiency Across QoL Categories END---













#1. Prepare the summary data from your model
# Odds Ratio = 1.41, 95% CI = [1.14, 1.74]
or_summary_data <- data.frame(
  # The comparison label
  Comparison = "No Deficiency vs. Deficiency", 
  Estimate = 1.41,
  Lower = 1.14,
  Upper = 1.74
)

# 2. Build the Forest Plot
ggplot(or_summary_data, aes(x = Estimate, y = Comparison)) +
  
  # A. Vertical Reference Line at the 'null' effect (OR = 1.0)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray60", linewidth = 1) +
  
  # B. Standard Error bars (Blue) representing the 95% CI
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), 
                 height = 0.2, color = "#FFC627", linewidth = 1.2) +
  
  # C. The Odds Ratio estimate point (Orange)
  geom_point(color = "#8C1D40", size = 6) +
  
  # D. Polished inner white dot
  geom_point(color = "white", size = 2) +
  
  # E. Precise numeric labels hovering directly over the point
  geom_text(aes(label = paste("OR: 1.41\n95% CI: [1.14, 1.74]")),
            vjust = -1.2, size = 4.5, fontface = "bold", color = "black") +
  
  # Formatting
  scale_x_continuous(limits = c(0.8, 2.0), breaks = c(1.0, 1.25, 1.5, 1.75, 2.0)) +
  theme_minimal() +
  labs(
    title = NULL, # No main title 
    subtitle = NULL, # No subtitle
    x = "Odds Ratio (Likelihood)",
    y = "" # Blank label because the text on the axis says it all
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 20)),
    axis.text.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    # Make the comparison label on the left axis bold and black
    axis.text.y = element_blank(), 
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)),
    # Remove clutter: vertical lines and minor gridlines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank()
  )
#---------- Forest Plot for Ordinal Logistic Regression Summary END ---



# 5.2.3. Calculate proportions and standard errors for the 95% CI error bars
# (Note: total_n is no longer needed since we calculate it per group below)

df_plot <- df_grouped %>%
  pivot_longer(cols = c(`Vitamin D Deficiency`, `No Deficiency`), 
               names_to = "VitaminD_Status", 
               values_to = "Count") %>%
  # 5.2.3.1. Group the data so calculations happen within each Vitamin D Status
  group_by(VitaminD_Status) %>% 
  mutate(
    # 5.2.3.2. Calculate the total 'n' for THIS specific group
    group_n = sum(Count),
    
    # 5.2.3.3. Relative frequency (proportion, p) WITHIN the group
    p = Count / group_n,
    
    # 5.2.3.4. Standard error using the specific group_n
    se = sqrt((p * (1 - p)) / group_n),
    
    # 5.2.3.5. Calculate 95% Confidence Intervals (p +/- 1.96 * SE)
    ci_lower = p - (1.96 * se),
    ci_upper = p + (1.96 * se)
  ) %>%
  ungroup() # Always good practice to ungroup when you are done!
