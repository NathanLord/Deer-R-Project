library(tidyverse)
library(ggplot2)
library(plotly) # For interactive part
library(ggrepel) # For overlapping of label/text


library(readxl)

file_path <- "AllKillsCty2022RegionalGrouping.xlsx"

wi_deer <- read_excel(file_path, sheet = "AllKillsUnitWithTribal")


view(wi_deer)



# Select the first column and the last four columns
wi_deer <- wi_deer[, c(1, (ncol(wi_deer) - 3):ncol(wi_deer))]


view(wi_deer)


# Specify the desired column order and names
desired_order_and_names <- c("County", "Antlered", "Antlerless", "Unknowns", "Total")

# Rename variables
colnames(wi_deer) <- desired_order_and_names


view(wi_deer)

# Get rid of unwanted rows
# "Apostle Islands," "Bad River," "Lac Courte Oreilles," "Lac du Flambeau," "MCCoy," "Madeline Island," "Red Cliff," 
wi_deer <- wi_deer %>%
  filter(!(is.na(County) | County %in% c("County", "Unknown", "Apostle Islands", "Bad River", 
                                         "Lac Courte Oreilles","Lac du Flambeau", "MCCoy", "Madeline Island", "Red Cliff")))


view(wi_deer)

# Convert chr columns to numeric dbl that are actually numeric
wi_deer <- wi_deer %>%
  mutate_at(vars(Antlered, Antlerless, Unknowns, Total), as.numeric)



view(wi_deer)


# Group by the "County" column and summarize other columns by summing
wi_deer <- wi_deer %>%
  group_by(County) %>%
  summarize(
    Antlered = sum(Antlered, na.rm = TRUE),
    Antlerless = sum(Antlerless, na.rm = TRUE),
    Unknowns = sum(Unknowns, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )


view(wi_deer)





# http://www.usa.com/rank/wisconsin-state--land-area--county-rank.htm 
county_sizes <- data.frame(
  County = c("Marathon", "Bayfield", "Marinette", "Douglas", "Sawyer", "Price", "Clark", "Dane", "Grant", 
             "Oneida", "Ashland", "Forest", "Chippewa", "Oconto", "Jackson", "Taylor", "Polk", "Rusk", 
             "Monroe", "Shawano", "Lincoln", "Dodge", "Langlade", "Barron", "Vilas", "Dunn", "Sauk", 
             "Burnett", "Portage", "Washburn", "Wood", "Vernon", "Juneau", "Columbia", "Iowa", "Iron", 
             "Waupaca", "Trempealeau", "ST CROIX", "Fond Du Lac", "Rock", "Buffalo", "Adams", 
             "Eau Claire", "Outagamie", "Lafayette", "Waushara", "Manitowoc", "Richland", "Green", 
             "Pierce", "Crawford", "Jefferson", "Walworth", "Waukesha", "Brown", "Sheboygan", "Florence", 
             "Door", "Marquette", "La Crosse", "Winnebago", "Washington", "Menominee", "Green Lake", 
             "Kewaunee", "Racine", "Calumet", "Kenosha", "Milwaukee", "Ozaukee", "Pepin"),
  Size = c(1544.98, 
           1477.86, 
           1399.35, 1304.14, 
           1257.3, 1254.37, 1209.81, 
           1197.24, 1146.85, 1112.97, 
           1045.03, 1014.07, 1008.37, 
           997.99, 987.72, 974.88, 913.96, 913.58, 900.77, 
           893.06, 878.97, 875.62, 870.64, 862.71, 856.6, 850.1, 830.9, 821.85, 800.68, 
           797.11, 793.12, 791.58, 766.92, 765.53, 762.58, 758.17, 747.71, 732.96, 722.33, 719.55, 718.14, 
           671.63, 645.65, 637.98, 637.52, 633.59, 626.15, 
           589.08, 586.15, 583.96, 573.75, 570.66, 556.47, 555.13, 549.57, 533.64, 511.27, 
           488.19, 481.98, 455.60, 451.69, 434.49, 430.70, 
           357.61, 349.44, 342.52, 332.50, 318.24, 
           271.99, 241.40, 233.08, 231.98)
)

# Combine the size data with the existing data frame
# Convert the "County" column in both datasets to only capitalfor first letter
wi_deer$County <- tolower(wi_deer$County)
wi_deer$County <- paste0(toupper(substring(wi_deer$County, 1, 1)), substring(wi_deer$County, 2))

county_sizes$County <- tolower(county_sizes$County)
county_sizes$County <- paste0(toupper(substring(county_sizes$County, 1, 1)), substring(county_sizes$County, 2))

# Merge wi_deer with county_sizes based on the County column
wi_deer <- merge(wi_deer, county_sizes, by = "County", all.x = TRUE)

# View the updated wi_deer dataset
View(wi_deer)




# Add a new column "State" with the value "WI" to wi_deer using mutate
wi_deer <- wi_deer |> mutate(State = "WI")

# View the updated wi_deer dataset
View(wi_deer)

# Add the ratio for total by the size
wi_deer$Ratio <- wi_deer$Total / wi_deer$Size


View(wi_deer)

# Get rid of unkown column
wi_deer <- wi_deer %>%
  select(-Unknowns)

View(wi_deer)

