library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(choroplethrMaps)

# Load lookup data for county FIPS
data(county.regions)

# 
intensity <- c(D0="Abnormally Dry", D1="Moderate Drought", D2="Severe Drought", 
               D3="Extreme Drought", D4="Exceptional Drought")
 
today <- format(Sys.Date(), "%Y%m%d")

# Access drought monitor data for one week (given today's date)
drought <-  
read_csv(sprintf("http://droughtmonitor.unl.edu/USDMStatistics.ashx/?mode=table&aoi=county&date=%s", today)) %>% 
  gather(drought_level, value, D0, D1, D2, D3, D4) %>% 
  mutate(intensity=factor(intensity[drought_level], 
                          levels=as.character(intensity), ordered=TRUE))


# Get drought data for many months. (This can be changed by modifying the 
# sequence of dates)
x <- seq.Date(from = as.Date("2010-08-01"), to = as.Date("2015-08-01"), by = "month")
x <- format(x, "%Y%m%d")

df <- data.frame(Week=as.Date(character()),
                 FIPS=integer(), 
                 County=character(), 
                 State=character(), 
                 Nothing=numeric(), 
                 ValidStart=as.Date(character()), 
                 drought_level=character(), 
                 value=numeric(),
                 intensity=character(),
                 stringsAsFactors=FALSE)

for (date in x) {
  drought <- 
    read_csv(sprintf("http://droughtmonitor.unl.edu/USDMStatistics.ashx/?mode=table&aoi=county&date=%s", date)) %>% 
        gather(drought_level, value, D0, D1, D2, D3, D4) %>% 
        mutate(intensity=factor(intensity[drought_level], 
                                levels=as.character(intensity), ordered=TRUE))
  df <- rbind(df, drought)
}

# I wanted to try and plot this data on a ggplot2 map, so I needed to work on 
# getting the fips lookups.

# Get map data for the US
state_df <- data.table(map_data("state"))
county_df <- data.table(map_data("county"))

# Ugh, the join is a mess because some of the counties are spelled differently!
# the first words are those that are found in the ggplot2 map county_df data.frame
# the second are what they should be gsub'd into so that we can do the lookup and 
# include the fips in the ggplot2 map county_df data.frame so that we can plot 
# the drought data. 

#"de kalb" <- "dekalb"
#"st" <- "st."
#"la porte" <- "laporte"
#"prince georges" <- "prince george's"
#"st louis city" <- "st. louis"
#"queen annes" <- "queen anne's"
#"la moure" <- "lamoure"
#"du page" <- "dupage"
#"obrien" <- "o'brien"
#"st marys" <- "st. mary's"
#"yellowstone national" <- "yellowstone"



# Get the FIPS lookup table to match with the map data for the US
counties <- data.table(county.regions)

# Some manual changes need to happen to be able to plot the counties on the 
# county map
setdiff(counties$county.name, county_df$subregion)
setdiff(county_df$subregion, counties$county.name)

# After the cleanup, you can merge the values
left_join(county_df, counties, by = c('subregion', 'county.name'))

choropleth <- merge(county_df, counties, by.x = "subregion", by.y = 'county.name')


# Some map code I found on R-bloggers
# get more here: 
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

choropleth <- choropleth[order(choropleth$order), ]
choropleth$rate_d <- cut(choropleth$rate, breaks = c(seq(0, 10, by = 2), 35))
ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) +
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  scale_fill_brewer(pal = "PuRd")

