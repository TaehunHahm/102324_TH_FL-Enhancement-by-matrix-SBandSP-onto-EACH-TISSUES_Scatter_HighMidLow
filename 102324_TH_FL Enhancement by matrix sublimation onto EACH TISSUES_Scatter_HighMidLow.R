# Install required packages if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
if (!requireNamespace("rlang", quietly = TRUE)) install.packages("rlang")
if (!requireNamespace("lifecycle", quietly = TRUE)) install.packages("lifecycle")
if (!requireNamespace("pillar", quietly = TRUE)) install.packages("pillar")

# Load required libraries
library(rlang)
library(lifecycle)
library(pillar)
library(ggplot2)
library(ggrepel)

# Set working directory first
setwd("C:/Users/friendethan-PC/OneDrive - Johns Hopkins/Documents")

# Verify the file exists
list.files(pattern = ".csv")

# Read the CSV with the exact name
Sublimation_Tissues <- read.csv("102324_Spraying_Tissues_scatter(FITC,TRITC,mCherry)liver.csv")


# Remove rows with missing values
Sublimation_Tissues <- Sublimation_Tissues[complete.cases(Sublimation_Tissues[, c('log2FoldEnhancement', 'pvalue', 'C', 'D')]), ]


# Define color based on log2FoldEnhancement
keyvals <- cut(Sublimation_Tissues$log2FoldEnhancement,
               breaks = c(-Inf, 0.9, 2, Inf),
               labels = c('low', 'mid', 'high'))

# Define labels
top_labels <- Sublimation_Tissues$C

# Define label colors
label_colors <- ifelse(grepl("^FITC", top_labels), "chartreuse4",
                       ifelse(grepl("^TRITC", top_labels), "red3",
                              ifelse(grepl("^mCherry", top_labels), "gold3", "brown")))

# Convert 'C' to a factor with specified levels
Sublimation_Tissues$C <- factor(Sublimation_Tissues$C, levels = c("FITC", "TRITC", "mCherry"))


# Rest of your plotting code remains largely the same
scatter_plot <- ggplot(Sublimation_Tissues, aes(x = C, y = log2FoldEnhancement, size = -log10(pvalue), color = keyvals)) +
  geom_point(alpha = 0.5, shape = 16) +  # Increased transparency and changed shape for better visibility
  scale_color_manual(values = c('low' = '#6495ED', 'mid' = '#FFA333', 'high' = '#DC143C'),  # More distinctive colors
                     name = "Enhancement Level") +
  scale_size(range = c(1, 12), name = "-Log10(p-value)") +  # Adjusted size range for better scaling
  geom_label_repel(
    data = Sublimation_Tissues,
    aes(label = D),
    size = 3.9,
    color = label_colors,
    fontface = 'bold',
    box.padding = unit(0.73, "lines"),
    point.padding = unit(0.1, "lines"),
    segment.color = label_colors,
    segment.size = 0.5,
    min.segment.length = 0.5,
    max.overlaps = 30
  ) +
  geom_hline(yintercept = c(-1, 1, 2), linetype = c('dashed', 'longdash', 'longdash'), 
             color = c('grey', '#6495ED', '#DC143C'), size = 0.5) +
  labs(
    title = "FL Enhancement by Matrix Spraying onto Livers",
    subtitle = "Differential Analysis of Fluorescence Channels",
    x = "Fluorescence Channels",
    y = "Log2 Fold Enhancement"
  ) +
  theme_minimal() +  # A cleaner theme
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#555555"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend()
  ) +
  # Adding a background annotation for regions of interest
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = 7, alpha = 0.1, fill = "#FFFF00") +
  annotate("text", x = 2, y = 5.0, label = "", hjust = 0.5, vjust = 1, color = "#DC143C")  # Use 2 for TRITC position

# Save plot
plot_title <- "New3"
ggsave(paste0("Spraying_Tissues_scatter(FITC,TRITC,mCherry)_", plot_title, ".TIFF"), plot = scatter_plot, width = 7, height = 7, units = "in", dpi = 300)

