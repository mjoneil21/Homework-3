# Prepare county level data frame for Michigan

library(dplyr)
library(rvest) # For web scraping

# Read in acs county data file
county_df <- read.csv(file='data/acs2015_county_data.csv')

# Read state abbreviation and region file
# Obtained from https://github.com/cphalpert/census-regions
states <- read.csv(file='data/us_census_bureau_regions_and_divisions.csv')

# Merge and then reorder columns to bring those from states to the left.
county_df <- merge(county_df, states, by="State")
new_col_order <- c(1:2, (ncol(county_df)-2):ncol(county_df), 3:(ncol(county_df)-3))
county_df <- county_df[,new_col_order]

# Save a csv version
write.csv(county_df, file = 'data/county.csv', row.names = FALSE)

# Filter to get Michigan records
county_MI <- filter(county_df, State == 'Michigan')

# Want to add a peninsula field containing LP or UP
# Found a Wikipedia page containing a table with UP counties and I'll
# scrape it from the page and get the counties into a vector I can
# use to help populate the peninsual field.

# Approach borrowed from 
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/.

url <- "https://en.wikipedia.org/wiki/Upper_Peninsula_of_Michigan"

# To get the xpath for the html table I browsed to the page and inspected
# the elements to find the html table tag. See blog post above for details.

up_counties <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/div/table[2]') %>%
  html_table()

# Result is a list containing a data frame. Let's pull
# out the data frame.
up_counties <- up_counties[[1]]

# Now let's get rid of the Total line and get the county names into a vector.
up_counties <- up_counties %>%
  filter(County != 'Total')

up_counties <- up_counties$County

# Create a data frame with MI counties from up_counties vector

mi_counties <- county_MI %>%
  select(County) %>%
  arrange(County)

mi_counties <- mi_counties %>%
  mutate(peninsula = if_else(County %in% up_counties, "UP", "LP"))

# Now merge the tables.
county_MI <- merge(county_MI, mi_counties, by="County")

## Reorder the columns so that peninsula is second column.
# Create a vector with desired column order.
new_col_order <- c(1, ncol(county_MI), 2:(ncol(county_MI)-1))
county_MI <- county_MI[,new_col_order]

#Save a csv version.
write.csv(county_MI, file = 'data/county_MI.csv', row.names = FALSE)

# Clean up
rm(mi_counties, up_counties, county_df, url, county_MI, states, new_col_order)


