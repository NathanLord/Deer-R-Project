library(tidyverse)
library(ggplot2)
library(plotly) # For interactive part
library(ggrepel) # For overlapping of label/text


# https://www.mdnr-elicense.com/HarvestReportSummary 
mi_deer <- read.csv("DeerHarvestSummaryReport.csv")


view(mi_deer)

summary(mi_deer)

# Removes the commas from the values, before on values under 1,000 were working
mi_deer$Total <- as.numeric(gsub(",", "", mi_deer$Total))
mi_deer$Antlered <- as.numeric(gsub(",", "", mi_deer$Antlered))
mi_deer$Antlerless <- as.numeric(gsub(",", "", mi_deer$Antlerless))

# Dont do this plot 
ggplot(mi_deer |> arrange(Total), aes(x = factor(County, levels = County), y = Total)) +
  geom_bar(stat = "identity", fill = County, alpha = 0.7) +
  labs(title = "Deer Harvest Summary by County",
       x = "County",
       y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a ggplot with gradient fill and make it so it is interactive
p <- ggplot(mi_deer |> arrange(Total), aes(x = factor(County, levels = County), y = Total, fill = Total)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_gradient(low = "red", high = "green", guide = guide_legend(title = "Total")) +
  labs(title = "Deer Harvest Summary by County",
       x = "County",
       y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p <- ggplotly(p, tooltip = c("y"))

p




# For upper peninsula only
upper_peninsula_counties <- c("Alger", "Baraga", "Chippewa", "Delta", "Dickinson", 
                              "Gogebic", "Houghton", "Iron", "Keweenaw", "Luce", 
                              "Mackinac", "Marquette", "Menominee", "Ontonagon", "Schoolcraft")

# Filter the data
# Could go about this a multitude of ways
deer_upper_peninsula <- mi_deer[grepl(paste(upper_peninsula_counties, collapse = "|"), mi_deer$County), ]

#same thing above as this 
# Filter the data ------
# deer_upper_peninsula <- mi_deer |>
#  filter(grepl(paste(upper_peninsula_counties, collapse = "|"), County))

p2 <- ggplot(deer_upper_peninsula %>% arrange(Total), aes(x = factor(County, levels = County), y = Total, fill = Total)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_gradient(low = "red", high = "green", guide = guide_legend(title = "Total")) +
  labs(title = "Deer Harvest Summary by County",
       x = "County",
       y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
p2 <- ggplotly(p2, tooltip = c("y"))

p2







# Lolipop time
p3 <- ggplot(mi_deer, aes(x = reorder(County, Antlered), y = Antlered)) +
  geom_segment(aes(xend = reorder(County, Antlered), yend = Antlerless),
               color = "black", size = 1) +
  geom_point(aes(y = Antlered), color = "blue", size = 3) +
  geom_point(aes(y = Antlerless), color = "pink", size = 3) +
  labs(title = "Lollipop Plot of Antlered and Antlerless by County",
       x = "County",
       y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p3 <- ggplotly(p3, tooltip = "text")


p3



# Make interactive
p4 <- ggplot(mi_deer, aes(x = reorder(County, Antlered), y = Antlered)) +
  geom_segment(aes(xend = reorder(County, Antlered), yend = Antlerless),
               color = "#694f43", size = 1) +
  geom_point(aes(y = Antlered, text = Antlered),
             color = "#91b2c1", size = 3) +
  geom_point(aes(y = Antlerless, text = Antlerless),
             color = "#f39ca1", size = 3) +
  labs(title = "Lollipop Plot of Antlered and Antlerless by County",
       x = "County",
       y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the ggplot to a plotly object
p4 <- ggplotly(p4, tooltip = "text")

# Display the interactive plot
p4




# http://www.usa.com/rank/michigan-state--land-area--county-rank.htm
# Add county size to the dataset
# List of sizes corresponding to each county
# Size is in sq miles
sizes <- c(1808.40, 1558.42, 1311.22, 1171.36, 1171.09, 1166.15, 1101.85, 1044.08, 1021.57, 1009.10,
           962.57, 915.07, 899.08, 898.26, 867.66, 846.95, 835.71, 825.23, 813.20, 803.13,
           800.11, 761.40, 749.55, 721.17, 715.26, 706.23, 705.96, 705.40, 701.67, 674.58,
           658.72, 643.01, 636.98, 612.08, 598.13, 575.17, 572.68, 571.86, 571.30, 568.46,
           567.75, 567.37, 566.41, 566.39, 565.73, 565.25, 565.00, 564.73, 564.31, 563.49,
           563.47, 563.15, 562.79, 561.66, 559.86, 556.28, 556.12, 555.07, 553.09, 549.39,
           549.10, 546.66, 542.15, 540.11, 530.67, 519.64, 516.25, 514.97, 512.07, 501.78,
           500.59, 499.25, 495.07, 490.06, 479.22, 475.70, 467.49, 464.33, 442.30, 416.34,
           363.19, 347.17, 319.70)

# Create a named vector with County as names
sizes_vector <- setNames(sizes, c("Marquette", "Chippewa", "Ontonagon", "Schoolcraft", "Delta",
                                  "Iron", "Gogebic", "Menominee", "Mackinac", "Houghton",
                                  "Sanilac", "Alger", "Luce", "Baraga", "Oakland", "Kent",
                                  "Huron", "Allegan", "Newaygo", "Tuscola", "Saginaw",
                                  "Dickinson", "Lenawee", "Saint Clair", "Cheboygan",
                                  "Calhoun", "Washtenaw", "Montcalm", "Jackson", "Alcona",
                                  "Presque Isle", "Lapeer", "Genesee", "Wayne", "Van Buren",
                                  "Hillsdale", "Eaton", "Isabella", "Alpena", "Ionia",
                                  "Gratiot", "Berrien", "Lake", "Clinton", "Osceola", "Oscoda",
                                  "Livingston", "Wexford", "Missaukee", "Clare", "Ogemaw",
                                  "Ottawa", "Kalamazoo", "Kalkaska", "Crawford", "Ingham",
                                  "Mecosta", "Barry", "Monroe", "Iosco", "Montmorency", "Manistee",
                                  "Keweenaw", "Shiawassee", "Roscommon", "Midland", "Otsego",
                                  "Oceana", "Branch", "Gladwin", "Saint Joseph", "Muskegon",
                                  "Mason", "Cass", "Macomb", "Antrim", "Emmet", "Grand Traverse",
                                  "Bay", "Charlevoix", "Arenac", "Leelanau", "Benzie"))

# Add the "Size" column to the deer data frame
mi_deer$Size <- sizes_vector[mi_deer$County]

# View the updated data frame
view(mi_deer)

# Add a new column "State" with the value "Mi" to mi_deer using mutate
mi_deer <- mi_deer |> mutate(State = "MI")

# View the updated wi_deer dataset
View(mi_deer)

# Proportion plot

# Calculate the ratio of Total to Size
mi_deer$Ratio <- mi_deer$Total / mi_deer$Size

# Create the plot with ordered counties
ggplot(mi_deer, aes(x = reorder(County, Ratio), y = Ratio)) +
  geom_bar(stat = "identity", fill = "#694f43", color = "black") +
  labs(title = "Total/Size Ratio by County",
       x = "County",
       y = "Total/Size Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# More comparison ideas
ggplot(mi_deer, aes(x = Size, y = Ratio)) +
  geom_point(color = "#694f43") +
  labs(title = "Deer Harvested per sq mi vs Size of County",
       x = "Size (sq mi)",
       y = "Deer Harvested per sq mi") +
  theme_minimal()


ggplot(mi_deer, aes(x = Size, y = Ratio, fill = Ratio)) +
  geom_point(shape = 21, color = "black", size = 3) +
  labs(title = "Deer Harvested per sq mi vs Size of County",
       x = "Size (sq mi)",
       y = "Deer Harvested per sq mi") +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal()


ggplot(mi_deer, aes(x = Size, y = Ratio, fill = Ratio, label = County)) +
  geom_point(shape = 21, color = "black", size = 3) +
  geom_text(position = position_nudge(y = 0.01), vjust = 0, hjust = -0.2) +
  labs(title = "Deer Harvested per sq mi vs Size of County",
       x = "Size (sq mi)",
       y = "Deer Harvested per sq mi") +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(mi_deer, aes(x = Size, y = Ratio, fill = Ratio, label = County)) +
  geom_point(shape = 21, color = "black", size = 3) +
  geom_text_repel(box.padding = 0.5, point.padding = 0.5) +
  labs(title = "Deer Harvested per sq mi vs Size of County",
       x = "Size (sq mi)",
       y = "Deer Harvested per sq mi") +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  theme(legend.position = "none")




# For the UP
ggplot(mi_deer |>filter(grepl(paste(upper_peninsula_counties, collapse = "|"), County)), 
       aes(x = Size, y = Ratio, fill = Ratio, label = County)) +
  geom_point(shape = 21, color = "black", size = 3) +
  geom_text_repel(box.padding = 0.5, point.padding = 0.5) +
  labs(title = "Deer Harvested per sq mi vs Size of County",
       x = "Size (sq mi)",
       y = "Deer Harvested per sq mi") +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  theme(legend.position = "none")
