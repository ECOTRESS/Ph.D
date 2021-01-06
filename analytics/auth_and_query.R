# Packs
#-------------------------

#install.packages("googleAuthR")
#install.packages("RGoogleAnalytics")

library(RGoogleAnalytics)
library(googleAuthR)
library(googleAnalyticsR)

# Auth
#-------------------------
ga_auth()

# Querying data
#-------------------------

account_list = ga_account_list()

# Change this to your own view ID, as the one below will only work for accessing
# data from Mark Edmondson's blog, and it's highly unlikely you have access to that!
account_list$viewId
my_id <- 215711203 

# Now, query for some basic data, assigning the data to a 'data frame' object 
# called 'web_data'
web_data = google_analytics(my_id, 
                               date_range = c("2020-06-01", "2020-12-30"),
                               metrics = c("sessions","pageviews", 
                                           "pageValue",
                                           "entrances","bounces"),
                               dimensions = c("date","deviceCategory",
                                              "channelGrouping"),
                               anti_sample = TRUE)


