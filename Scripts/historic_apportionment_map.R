library(tidyverse)
library(ggthemes)

data.frame("year" = c(1790,1800,1810,1820,1830,1840,1850,1860,1870,1880,1890,
                      1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,
                      2010,2020),
           "seats" = c(105,141,181,213,240,223,234,241,292,325,356,386,435,
                       435,435,435,435,435,435,435,435,435,435,435)) %>%
  ggplot(aes(x = year, y = seats)) +
  geom_vline(xintercept = 1929, size = 1, color = "grey") +
  geom_line(size = 1.5) +
  scale_x_continuous(limits = c(1790,2020)) +
  labs(title = "The Number of Seats in the House of Representatives",
       subtitle = "From the 1790 apportionment to the 2020 apportionment",
       x = "Year",
       y = "Seats in the House",
       caption = "Historical House seat counts from history.house.gov") +
  theme_clean() +
  annotate("text", x = 1890, y = 135, label = "Permanent Apportionment Act 
           of 1929 signed into law")
  
