rm(list = ls())

library(tidyverse)
library(scales)
library(ggpubr)

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

# Spaghetti plot of arrival rate (average number of flood events per year) for each gage
spaghetti_plot_arrival_rate <- function(eventCountDfByFloodMechanism, regionName, thisMechanism, xAxisBreaks, xAxisLabels, meanRate) {
  
  spaghetti <- ggplot() +
    geom_line(eventCountDfByFloodMechanism, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId))) +
    geom_point(eventCountDfByFloodMechanism, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId)), size = 1, shape = 21, stroke = 2) +
    geom_line(meanRate, mapping = aes(x = as.numeric(StartDate), y = Rate), size = 1.5) +
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


# Directory and pilot region information here -----------------------------

# Your file directory and region name here
inputFile <- "C:/Projects/MixedPopulations/TASK5/DelawareRiver/Data/DelawareRiverPDSEventCountSummary_10years.csv"
outputDir <- "C:/Projects/MixedPopulations/TASK6/DelawareRiver/PDS_Figures/"
regionName <- "Delaware River"
timePeriod <- " 10 years "
minNumberYearsDailyData <- 2 # Suggest ~25% coverage (e.g. for a 20 year time period, use 5 years and for a 10 year time period, use 2-3 years)

# Spaghetti Plots of arrival rate -----------------------------------------

eventCountDf <- read.csv(inputFile) %>%
  mutate(GageId = paste0("0", GageId), # Remove this line for regions with gages that do not have a leading 0
    StartDate = as.Date(StartDate), 
    EndDate = as.Date(EndDate) - 1, # Set to 30 September
    FloodMechanism = trimws(FloodMechanism),
    Rate = ifelse(NumberYears > 0, Count/NumberYears, NA)) 

floodMechanisms <- unique(eventCountDf$FloodMechanism)

# Create and save spaghetti plot of arrival rates for each flood mechanism
for (i in seq(length(floodMechanisms))) {
  
  # Subset dataframe to current flood mechanism
  thisMechanism <- floodMechanisms[i]
  eventCountDfByFloodMechanism <- eventCountDf[eventCountDf$FloodMechanism == thisMechanism,]
  
  # Create x-axls labels
  # e.g. "1900-10-01 - 1920-10-01" with a carriage return after the dash
  xAxisLabels <- create_date_range_labels(eventCountDfByFloodMechanism)
  xAxisBreaks <- unique(as.numeric(eventCountDfByFloodMechanism$StartDate))
  
  # Compute mean rate by time period for this flood mechanism
  thisMeanRate <- eventCountDfByFloodMechanism %>%
    group_by(StartDate, EndDate) %>%
    summarise(Rate = mean(Rate, na.rm = TRUE)) %>%
    mutate(GageId = "Average",
           FloodMechanism = thisMechanism,
           NumberYears = 999,
           Count = 999)
  
  # Bind dataframes together
  eventCountDfByFloodMechanism <- rbind(eventCountDfByFloodMechanism, thisMeanRate)
  
  # Set colors for each trace in spaghetti plot. Set average line to black.
  gageIds <- sort(unique(eventCountDfByFloodMechanism$GageId))
  colors <- setNames(scales::pal_hue()(length(gageIds)), gageIds[-length(gageIds)+1])
  colors["Average"] <- "black"
  
  # Generate spaghetti plot of arrival rate
  spaghetti_plot <-  spaghetti_plot_arrival_rate(eventCountDfByFloodMechanism, regionName, thisMechanism, xAxisBreaks, xAxisLabels, thisMeanRate)
  spaghetti_plot
  
  # Save spaghetti plot
  thisMechanism <- gsub("/", "", thisMechanism) # Remove / char from "TC/TSR" to avoid creating a new directory when saving the plot
  figureName <- paste0(regionName, timePeriod, " Spaghetti Plot Arrival Rate ", thisMechanism)
  
  save_plot(outputDir, figureName, spaghetti_plot)
  
  # Build a dataframe of mean arrival rate values for each flood mechanism
  if (i > 1) {
    meanRate <- bind_rows(lastMeanRate, thisMeanRate)
    lastMeanRate <- meanRate
  } else {
    lastMeanRate <- thisMeanRate
  }
}


# Spaghetti plot of arrival rate for all gages, regardless of flood mechanism --------

# Compute mean arrival rate of floods (regardless of flood mechanism) for each time period for each gage
eventCountDfAllFloodMechanisms <- eventCountDf %>%
  group_by(GageId, StartDate) %>%
  summarise(Rate = mean(Rate, na.rm = TRUE))

# Compute mean arrival rate of floods for each time period
meanRateAllGagesAllFloodMechanisms <- eventCountDfAllFloodMechanisms %>%
  group_by(StartDate) %>%
  summarise(Rate = mean(Rate, na.rm = TRUE)) %>%
  mutate(GageId = "Average",
         FloodMechanism = thisMechanism,
         NumberYears = 999,
         Count = 999)

eventCountDfAllFloodMechanisms <- rbind(eventCountDfAllFloodMechanisms, meanRateAllGagesAllFloodMechanisms)

gageIds <- sort(unique(eventCountDfAllFloodMechanisms$GageId))
colors <- setNames(scales::pal_hue()(length(gageIds)), gageIds[-length(gageIds) + 1])
colors["Average"] <- "black"

spaghetti_plot_all_gages <- ggplot() +
  geom_line(eventCountDfAllFloodMechanisms, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId))) +
  geom_point(eventCountDfAllFloodMechanisms, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(GageId)), size = 1, shape = 21, stroke = 2) +
  geom_line(meanRateAllGagesAllFloodMechanisms, mapping = aes(x = as.numeric(StartDate), y = Rate), size = 1.5) +
  geom_point(meanRateAllGagesAllFloodMechanisms, mapping = aes(x = as.numeric(StartDate), y = Rate), size = 1, shape = 21, stroke = 2) +
  scale_color_manual(values = colors) +
  xlab("Time Period") +
  ylab("Arrival Rate (events/year)") +
  labs(color = "Gage ID") +
  ggtitle(paste0(regionName, ": ", "All Flood Mechanisms")) +
  scale_x_continuous(breaks = xAxisBreaks, labels = xAxisLabels) +
  theme_bw()
spaghetti_plot_all_gages

figureName <- paste0(regionName, timePeriod, " Spaghetti Plot Arrival Rate All Floods")
save_plot(outputDir, figureName, spaghetti_plot_all_gages)

# Line plot of average arrival rate by flood mechanism --------------------

line_plot_average_all_mechanisms <- ggplot(meanRate, mapping = aes(x = as.numeric(StartDate), y = Rate, color = as.factor(FloodMechanism))) +
  geom_line(size = 1) +
  geom_point(size = 1, shape = 21, stroke = 2) +
  scale_x_continuous(breaks = xAxisBreaks, labels = xAxisLabels) +
  xlab("Time Period") +
  ylab("Arrival Rate (events/year)") +
  labs(color = "Flood Mechanism") +
  ggtitle(regionName) +
  ylim(0, 1.2) + # Might need to change upper limit
  theme_bw()
line_plot_average_all_mechanisms

figureName <- paste0(regionName, timePeriod, " Line Plot Average Arrival Rate By Flood Mechanism")
save_plot(outputDir, figureName, line_plot_average_all_mechanisms)

# Bar chart of arrival rate by time period --------------------------------

facetLabels <- c()

# Get mean number of years of daily data for gages in each time period
meanNumberOfYears <- eventCountDf %>%
  group_by(StartDate) %>%
  summarise(mean_value = mean(NumberYears, na.rm = TRUE))

# Create facet labels
for (i in seq(nrow(meanNumberOfYears))) {
  numYearsLabel <- paste0("\n\n Average number \nof years of\n daily data: ", round(meanNumberOfYears$mean_value[i], 1))
  thisLabel <- paste0(xAxisLabels[i], numYearsLabel)
  facetLabels <- c(facetLabels, thisLabel)
}

eventCountDf <- eventCountDf %>%
  mutate(StartDateLabel = "")

eventCountDf$StartDateLabel <- factor(eventCountDf$StartDate, labels = facetLabels)

# Facet bar chart
barChartByTimePeriod <- ggplot(eventCountDf, aes(x = FloodMechanism, y = Rate, fill = FloodMechanism)) + 
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
  ylab("Arrival Rate (events/year)")
barChartByTimePeriod

figureName <- paste0(regionName, timePeriod, " Bar Chart of Flood Mechanism Arrival Rate By Time Period")
save_plot(outputDir, figureName, barChartByTimePeriod)


# Pie chart of % of each flood mechanism by time period -------------------

# Get count of each flood type for each time period, count of all floods for each time period, and % of floods by flood mechanism in each time period
floodTypeCounts <- eventCountDf %>%
  group_by(StartDate, EndDate, FloodMechanism) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  group_by(StartDate, EndDate) %>%
  mutate(Total_Count = sum(Count, na.rm = TRUE),
         pct = Count / Total_Count) %>%
  arrange(pct) %>%
  mutate(labels = scales::percent(pct, accuracy = 0.1)) %>%
  ungroup()

# Get vector of unique start dates in increasing order (oldest to new)
startDates <- floodTypeCounts %>%
  select(StartDate)
startDates <- sort(unique(unlist(startDates))) 

pieChartList <- list()

for (i in seq(length(startDates))) {
  
  # Flood counts and percentages for the current time periods
  thisPeriodFloodTypeCounts <- floodTypeCounts[floodTypeCounts$StartDate == startDates[i],]
  startDate <- format(thisPeriodFloodTypeCounts$StartDate, "%d %b %Y") # Format date "01 Oct 1900"
  endDate <- format(thisPeriodFloodTypeCounts$EndDate, "%d %b %Y")
  
  title <- paste0(startDate, " -\n ", endDate)
  pieChartList[[i]] <- pie_chart_flood_type(thisPeriodFloodTypeCounts, title)
}

nCol <- length(startDates) / 2
# Arrange plots in grid
arrangedPieCharts <- do.call(ggarrange, c(pieChartList, nrow = 2, ncol = nCol, common.legend = TRUE, legend = "right")) 

# Add a title
pieChartTitle <- paste0(regionName, " Partial Duration Series Flood Mechanisms By Time Period")
arrangedPieCharts <- annotate_figure(arrangedPieCharts, top = text_grob(pieChartTitle, 
                                                                color = "black", 
                                                                face = "bold", 
                                                                size = 14)) +
  theme_bw() +
  theme(panel.border = element_blank())

arrangedPieCharts

figureName <- paste0(regionName, timePeriod, " PDS Pie Chart")
save_plot(outputDir, figureName, arrangedPieCharts)

