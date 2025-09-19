rm(list = ls())

library(tidyverse)

# Functions ---------------------------------------------------------------

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

# Directory and pilot region information here -----------------------------

# Your file directory and region name here
inputFile <- "C:/Projects/MixedPopulations/TASK5/IowaRiver/IowaRiverPDSMeanExcessSummary_10years.csv"
outputDir <- "C:/Projects/MixedPopulations/TASK6/IowaRiver/PDS_Figures/"
regionName <- "Iowa River"
timePeriod <- " 10 years "

# Bar chart of arrival rate by time period --------------------------------

meanExcessDf <- read.csv(inputFile) %>%
  mutate(StartDate = as.Date(StartDate), 
         EndDate = as.Date(EndDate) - 1, # Set to 30 September
         FloodMechanism = trimws(FloodMechanism)) 

meanExcessDf <- meanExcessDf[meanExcessDf$StartDate > as.Date("1940-09-30"), ]

facetLabels <- create_date_range_labels(meanExcessDf)
meanExcessDf$StartDateLabel <- factor(meanExcessDf$StartDate, labels = facetLabels)

# Facet bar chart
barChartByTimePeriod <- ggplot(meanExcessDf, aes(x = FloodMechanism, y = MeanExcess, fill = FloodMechanism)) + 
  geom_bar(stat = "summary") +
  facet_grid(. ~ StartDateLabel) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12), 
        strip.background = element_rect(color="black", linewidth = 1.5),
        axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1)) +
  labs(fill = "Flood Mechanism",
       title = regionName) +
  xlab("") +
  ylab("Mean Excess (cfs)")
barChartByTimePeriod

figureName <- paste0(regionName, timePeriod, " Bar Chart of Flood Mechanism Mean Excess By Time Period")
save_plot(outputDir, figureName, barChartByTimePeriod)
