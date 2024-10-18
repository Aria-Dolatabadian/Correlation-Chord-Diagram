# Load necessary libraries
library(ggplot2)
library(reshape2)
library(ggplotify)

# Read the data from the CSV file
coefficients <- read.csv("data.csv", row.names = 1)

# Convert to a long format for ggplot
coefficients_melted <- melt(as.matrix(coefficients))
colnames(coefficients_melted) <- c("Metabolite", "Clinical_Index", "Coefficient")  # Renaming the columns

# Create the heatmap
heatmap_plot <- ggplot(coefficients_melted, aes(x = Clinical_Index, y = Metabolite)) +
  geom_tile(aes(fill = Coefficient), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Coefficient") +
  theme_minimal() +
  coord_polar(theta = "y") +  # Convert to circular layout
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 10, face = "bold"),  # Rotate x-axis labels, change font
    axis.text.y = element_text(size = 10, face = "italic"),  # Adjust font of y-axis labels
    axis.title = element_blank(),  # Remove axis titles
    panel.grid = element_blank(),  # Remove grid lines
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and stylize title
    legend.position = c(0.01, 0.2),  # Manually position the legend (x=0.9, y=0.2)
    legend.title = element_text(size = 12, face = "bold"),  # Adjust legend title font
    legend.text = element_text(size = 10)  # Adjust legend text font
  ) +
  labs(title = "Circular Heatmap of Metabolite-Clinical Index Associations")  # Add a title to the plot

# Convert to a circular heatmap
circular_heatmap <- ggplotify::as.ggplot(heatmap_plot)

# Display the plot
print(circular_heatmap)

# Optional: Save the plot
ggsave("circular_heatmap_associations.jpg", plot = circular_heatmap)
