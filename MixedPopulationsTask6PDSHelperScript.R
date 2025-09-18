library(tidyverse)

# Save a plot object to the specified directory
save_plot <- function(dir, figureName, plot) {
  
  filename <- paste0(dir, figureName, ".png")
  
  ggsave(filename = filename,
         plot = plot,
         device = "png",
         units = "px",
         width = 1200,
         height = 800,
         dpi = 100)
}

# Spaghetti plot of arrival rate (average number of flood events per year) for each gage
spaghetti_plot_arrival_rate <- function(eventCountDfByFloodMechanism, regionName, thisMechanism, xAxisBreaks, xAxisLabels, meanRate) {
  
  spaghetti <- ggplot() +
    geom_line(eventCountDfByFloodMechanism, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId))) +
    geom_point(eventCountDfByFloodMechanism, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId)), size = 1, shape = 21, stroke = 2) +
    geom_line(meanRate, mapping = aes(x = as.numeric(StartDate), y = Rate), linewidth = 1.5) +
    geom_point(meanRate, mapping = aes(x = as.numeric(StartDate), y = Rate), size = 1, shape = 21, stroke = 2) +
    scale_color_manual(values = colors) +
    xlab("Time Period") +
    ylab("Arrival Rate (events/year)") +
    labs(color = "Gage ID") +
    ggtitle(paste0(regionName, ": ", thisMechanism)) +
    scale_x_continuous(breaks = xAxisBreaks, labels = xAxisLabels) +
    theme_bw()
  
  spaghetti
}

# Create x-axis tic mark labels corresponding to the time period ranges
# e.g. "1900-10-01 - 1920-10-01" with a carriage return after the dash
create_date_range_labels <- function(eventCountDfByFloodMechanism) {
  
  dateRanges <- eventCountDfByFloodMechanism %>%
    select(StartDate, EndDate)
  dateRanges <- unique(dateRanges)
  
  xAxisLabels <- c()
  for (i in seq(nrow(dateRanges))) {
    thisDateRange <- paste(dateRanges[i,]$StartDate, "-\n", dateRanges[i,]$EndDate) 
    xAxisLabels <- c(xAxisLabels, thisDateRange)
  }
  
  xAxisLabels
}

pie_chart_flood_type <- function(thisPeriodFloodTypeCounts, title) {
  
  subtitle <- paste0("PDS Event Count \n", thisPeriodFloodTypeCounts$FloodMechanism[1], ": ", thisPeriodFloodTypeCounts$Count[1], "\n",
                     thisPeriodFloodTypeCounts$FloodMechanism[2], ": ", thisPeriodFloodTypeCounts$Count[2], "\n",
                     thisPeriodFloodTypeCounts$FloodMechanism[3], ": ", thisPeriodFloodTypeCounts$Count[3], "\n",
                     "Total: ", thisPeriodFloodTypeCounts$Total_Count[1], "\n")
  
  pie <- ggplot(thisPeriodFloodTypeCounts, aes(x = "", y = pct, fill = FloodMechanism)) +
    geom_col(color = "white")  + 
    geom_text(aes(label = labels),
              color = "white",
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),     # Remove gridlines
      panel.border = element_blank(),
      axis.title = element_blank(),     # Remove axis titles
      axis.text = element_blank(),      # Remove axis text (numbers)
      axis.ticks = element_blank(),     # Remove axis ticks
    ) +
    ggtitle(title) +
    labs(subtitle = subtitle)
  
  pie
}