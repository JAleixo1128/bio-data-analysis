# Plankton Diversity Analysis in Narragansett Bay
# Uses Simpson’s Diversity Index to compare sites and tide conditions

library(gsheet)
library(ggplot2)
library(dplyr)

# Load class data
url_class <- 'https://docs.google.com/spreadsheets/d/1iiirObM-FBJjKvAvLMBsc1Ms-196t9MAo3G6dV0p1ks/edit?gid=604983628#gid=604983628'
div_data <- gsheet2tbl(url_class)

# Summary statistics
div_data %>%
  group_by(SiteType) %>%
  summarise(
    n = n(),
    min = min(Diversity, na.rm = TRUE),
    median = median(Diversity, na.rm = TRUE),
    max = max(Diversity, na.rm = TRUE)
  )

# Fox Island boxplot
fox <- filter(div_data, SiteType %in% c("FOX_incoming","FOX_outgoing"))
ggplot(fox, aes(x = SiteType, y = Diversity)) +
  geom_boxplot() +
  labs(x = "Fox Island Tide", y = "Simpson Diversity Index")

# GSO Dock boxplot
gso <- filter(div_data, SiteType %in% c("GSO_incoming","GSO_outgoing"))
ggplot(gso, aes(x = SiteType, y = Diversity)) +
  geom_boxplot() +
  labs(x = "GSO Dock Tide", y = "Simpson Diversity Index")

# Long-term data
url_long <- 'https://docs.google.com/spreadsheets/d/1RdEzJqLi1vRcpR80nH__0n7nJJ8gPZxoxxuHQqvtbWs/edit?gid=0#gid=0'
long <- gsheet2tbl(url_long)

ggplot(long, aes(x = Tide, y = Diversity)) +
  geom_boxplot() +
  labs(x = "Fox Island Tide", y = "Simpson Diversity Index")

# Monthly comparison
months <- filter(long, Month %in% c("January","May","June"))
ggplot(months, aes(x = Month, y = Diversity)) +
  geom_boxplot() +
  labs(x = "Month", y = "Simpson Diversity Index")

