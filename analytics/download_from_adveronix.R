library(googlesheets4)

campanhas = read_sheet("https://docs.google.com/spreadsheets/d/1mMZRGQnFd7YfLAtOrKYfOspXuCDPDKUASfa-N17Ano4/edit#gid=0",
                       sheet = 1)
face = read_sheet("https://docs.google.com/spreadsheets/d/1mMZRGQnFd7YfLAtOrKYfOspXuCDPDKUASfa-N17Ano4/edit#gid=0",
                  sheet = 2)

campanhas_final = as.data.frame(campanhas)
face_final = as.data.frame(face)

WriteXLS::WriteXLS(x = campanhas_final,ExcelFileName = "google.xls")

WriteXLS::WriteXLS(x = face_final,ExcelFileName = "face.xls")


# Work around latter
# # set own oauth app
# gads_auth_configure(path = 'C:/auth/app.json')
# # set your developer token if needed, or use default developer token
# gads_auth(email = 'me@gmail.com', developer_token = "own developer token")
# 
# # get list of accessible accounts
# my_accounts <- gads_get_accessible_customers()
# 
# # set manager account id (options)
# gads_set_login_customer_id('xxx-xxx-xxxx')
# 
# # set client account id
# gads_set_customer_id('xxx-xxx-xxxx')

# client id 905805892076-ocs14p2o5t2nllo2qdibitno3aq74tha.apps.googleusercontent.com
# client secret GhT667NQ6FXQi0gnfUJWqHMc
# token 6ottDCuRA132JxEt8RODgg

# adwords account id 285-820-2533
# client id (desktop app) 905805892076-sfckah47f8jum15nbkdt9daib0jvqqc1.apps.googleusercontent.com
# client secret oTdFXMtEwWQvKj0wgP7_dfvy
# developer token 6ottDCuRA132JxEt8RODgg
# client token 4/1AX4XfWjhTUHzwFp8bVwyyjN9b4cVgrq1jlUOtXkBbQ0El_5qqio5sy6-Cb0

#client_id = "905805892076-sfckah47f8jum15nbkdt9daib0jvqqc1.apps.googleusercontent.com"

# https://console.cloud.google.com/apis/dashboard 

#' google_oauth <-doAuth()
#' #' refreshToken(google_oauth)
#' #' 
#' #' @system("rm .google.auth.RData") #exclud auth
#' #' 
#' #See possible reports
#' reports()
#' 
#' #See metrics for a single report (e.g. campaign performance
#' metrics(report='CAMPAIGN_PERFORMANCE_REPORT')
#' 
#' #Query a single campaign performance
#' campaign_performance = statement(select=c("Date",
#'                                           # "AccountDescriptiveName",
#'                                           # "CampaignName",
#'                                           # "Impressions",
#'                                           "Clicks",
#'                                           "Cost",
#'                                           "Conversions"
#'                                           ),
#'                                  report="CAMPAIGN_PERFORMANCE_REPORT",
#'                                  start="2021-06-06",
#'                                  end="2021-06-10")
#' 
#' #See your adwords data in r using getData()
#' data <- getData(clientCustomerId='822-633-1439', #lokprinter # #Put your AdWords Account ID and not your Google Ads Manager Account.
#'                 google_auth=google_oauth,
#'                 statement=campaign_performance,
#'                 transformation = T, #data are transformed from xml text to R dataframe
#'                 changeNames = T)
#' 
#' 
#' #library(dygraphs)
#' # Import data
#' #setwd("/Users/usuario/Desktop/Lucas/Expandir_Digital_GoogleAds")
#' 
#' # designate project-specific cache
#' options(gargle_oauth_cache = ".secrets")
#' 
#' # check the value of the option, if you like
#' gargle::gargle_oauth_cache()
#' 
#' # trigger auth on purpose --> store a token in the specified cache
#' gs4_auth(email = "lucas.engenharia.dados@gmail.com")
#' # see your token file in the cache, if you like
#' list.files(".secrets/")
#' # 
# # Workround to make automatic download from googlesheet
#campanhas = read_sheet("https://docs.google.com/spreadsheets/d/1mMZRGQnFd7YfLAtOrKYfOspXuCDPDKUASfa-N17Ano4/edit#gid=0",
#                       sheet = 1)
# face = read_sheet("https://docs.google.com/spreadsheets/d/1mMZRGQnFd7YfLAtOrKYfOspXuCDPDKUASfa-N17Ano4/edit#gid=0",
#                 sheet = 2)
#' # 
#' # # trigger auth on purpose --> store a token in the specified cache
#gs4_deauth()
# # Save pro app funcionar
# WriteXLS::WriteXLS(c,ExcelFileName = "campanhas.xlsx")
# WriteXLS::WriteXLS(f,ExcelFileName = "face.xlsx")
# 
