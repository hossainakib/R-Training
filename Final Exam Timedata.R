getwd()


library(ggplot2)


Timedata <- read.csv(file.choose(), header = TRUE)

# verify structure
print(head(Timedata))
print(names(Timedata))

# Cleaning column names by trimming spaces
names(Timedata) <- trimws(names(Timedata))

# Converting `Date Time` column to proper date-time format
# Make sure the format matches the dataset (e.g., "11/25/2024 22:43")
Timedata$`Date.Time` <- as.POSIXct(Timedata$`Date.Time`, format = "%m/%d/%Y %H:%M")

# structure after conversion
print(str(Timedata))

# line graph
linegraph <- ggplot(Timedata, aes(x = `Date.Time`)) +
  geom_line(aes(y = Humidity, color = "Humidity (%)")) +
  geom_line(aes(y = `Air.Temperature`, color = "Air Temperature (°C)")) +
  labs(
    title = "Line Graph of Humidity and Air Temperature",
    x = "Date Time",
    y = "Values",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

linegraph
