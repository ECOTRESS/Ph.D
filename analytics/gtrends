### Comunication with googleTrends

install.packages("gtrendsR")
library(gtrendsR)
library(tidyverse)
res = gtrends(c("jeunesse"), geo = c("BR"), time = "today 12-m")

ibr = res$interest_by_region

ibr %>% 
  ggplot() + aes(x = location, y = hits, fill = location) +
  geom_col() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
