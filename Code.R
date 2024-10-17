# Load libraries
library(circlize)
library(Hmisc)
library(reshape2)
library(ComplexHeatmap)

# Step 1: Reading the CSV file
data <- read.csv("TEST.csv", header = TRUE, row.names = NULL)

# Step 2: Ensure the dataset contains only numeric columns
data <- data[, sapply(data, is.numeric)]

# Step 3: Pearson correlation and p-values calculation
cor_results <- rcorr(as.matrix(data), type = "pearson")

# Extracting correlation coefficients and p-values
cor_coeff <- cor_results$r  # Correlation coefficients
p_values <- cor_results$P   # P-values

# Handle missing values in the correlation matrix (replace NA with 0 for visualization purposes)
cor_coeff[is.na(cor_coeff)] <- 0
p_values[is.na(p_values)] <- 1  # Set NA p-values to 1 (non-significant)

# Export the correlation coefficients and p-values to CSV
write.csv(cor_coeff, "correlation_coefficients.csv")
write.csv(p_values, "p_values.csv")

# Step 4: Prepare the correlation matrix for visualization
diag(cor_coeff) <- NA  # Remove self-correlations
cor_melt <- melt(cor_coeff)
colnames(cor_melt) <- c("Trait1", "Trait2", "Correlation")
cor_melt <- cor_melt[cor_melt$Trait1 != cor_melt$Trait2, ]  # Remove self-correlations
cor_melt <- cor_melt[!duplicated(t(apply(cor_melt[, 1:2], 1, sort))), ]  # Remove duplicate correlations
cor_melt <- cor_melt[abs(cor_melt$Correlation) > 0.1, ]  # Filter out near-zero correlations

# Define correlation limits for color transparency
col_fun <- colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))

# Set transparency based on correlation magnitude
cor_melt$Transparency <- 1 - abs(cor_melt$Correlation)

# Step 5: Define custom sector colors for each trait
sector_colors <- structure(c("#FF6347", "#4682B4", "#3CB371", "#FFD700", "#FF69B4", "#87CEEB", "#9370DB", "#4682B4", "#3CB371", "#FFD700", "#FF69B4", "#87CEEB"),
                           names = unique(c(cor_melt$Trait1, cor_melt$Trait2)))

# Step 6: Create the circos plot with adjustable sector thickness, custom colors, and rotated labels
circos.clear()
circos.par(gap.degree = 3, start.degree = 90)  # Gap between sectors and start angle

# Customize sector thickness and rotation

track_height <- 0.1  
label_facing <- "inside"  # Direction of labels; can change to "reverse.clockwise" or "inside"
label_font_size <- 1  # Font size for labels (adjustable)
label_adjustment <- c(0, -2.5)  # Adjust label alignment (horizontal and vertical)

# Step 6a: Remove periods from trait names before plotting
cor_melt$Trait1 <- gsub("\\.", " ", cor_melt$Trait1)  # Replace periods with spaces
cor_melt$Trait2 <- gsub("\\.", " ", cor_melt$Trait2)

chordDiagram(
  x = cor_melt[, c("Trait1", "Trait2")],
  transparency = cor_melt$Transparency,
  col = col_fun(cor_melt$Correlation),
  annotationTrack = "grid",
  grid.col = sector_colors  # Apply custom colors to sectors
)

# Add trait names (labels) on the outer section with rotation control
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1] + 0.2,  # Adjust the label distance from the sector
      CELL_META$sector.index,
      facing = label_facing,
      niceFacing = TRUE,
      adj = label_adjustment,
      cex = label_font_size  # Adjust font size
    )
  },
  bg.border = NA, track.height = track_height  # Adjust track height for sector thickness
)

# Step 7: Add heatmap legend for correlation values with customizable positioning
lgd <- Legend(
  col_fun = col_fun,
  title = "Correlation",
  at = c(-1, 0, 1),
  labels = c("-1", "0", "1"),
  title_position = "topcenter",
  direction = "horizontal"
)

draw(lgd, x = unit(0.8, "npc"), y = unit(0.2, "npc"), just = c("right", "bottom"))

# Step 8: Export the plot as a high-resolution image
png("circos_plot_with_legend_high_res.png", width = 3000, height = 3000, res = 300)  # Increased resolution

# Clear any previous circos setup
circos.clear()

# Create the circos plot with adjustable sector thickness, custom colors, and rotated labels
chordDiagram(
  x = cor_melt[, c("Trait1", "Trait2")],
  transparency = cor_melt$Transparency,
  col = col_fun(cor_melt$Correlation),
  annotationTrack = "grid",
  grid.col = sector_colors  # Apply custom colors to sectors
)

# Add trait names (labels) on the outer section with rotation control inside the export block
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1] + 0.2,  # Adjust the label distance from the sector
      CELL_META$sector.index,
      facing = label_facing,
      niceFacing = TRUE,
      adj = label_adjustment,
      cex = label_font_size  # Adjust font size inside the export block
    )
  },
  bg.border = NA, track.height = track_height  # Adjust track height for sector thickness
)

# Draw the heatmap legend
draw(lgd, x = unit(1, "npc") - unit(10, "mm"), y = unit(1, "npc") - unit(10, "mm"), just = c("right", "top"))

# Finish exporting the image
dev.off()
