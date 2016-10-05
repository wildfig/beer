# Varsity Social Analysis Data Gathering
# Author: Zach Bricker
# Organization: WildFig Data

# Load Library's
libs <- c('Rfacebook', 'twitteR')
lapply(libs, library, character.only = TRUE)
#imports (says to interface that you are using methods from libraries)
remove(libs)

# Twitter Auth
api_key <- "yAqBhP6RSfRunXDoO4ee25QWG"
api_secret <- "c88D4aeHiq3qmdiQNNZhwywZlCFU15vREnDaoU3qXsplxgAlTT"
#password
access_token <- "14147533-tTaGKN6AuWhiRTr5GU9kXKgJjbBCxENNfAzzn5VZE"
#you as a person
access_token_secret <- "0NduDQBXpULYThs3CLPlgigAKMzkMoJL1UTRB53bho6VR"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

1

# FB Auth
#fb_OAuth <- fbOAuth(app_id = '1737964996471659', app_secret = 'f2f1b2c989ceda6e0763dd661ca54981', extended_permissions = TRUE)
#save(fb_OAuth, file = 'fb_OAuth')
load('fb_OAuth')
# Setup Client Objects
facebook_names <- c('labattusa', 'MolsonCanadian', 'MichelobULTRA', 'BudLight')
twitter_names <- c('LabattUSA', 'Molson_Canadian', 'MichelobULTRA', 'budlight')
years <- c('2011', '2012', '2013', '2014', '2015', '2016')
#Date ranges on FB

# GetPages Function for Facebook
fbGetPages <- function(vendor, svar, year, nposts){
  print(c(vendor, svar, year, nposts))
  getPage(page = vendor, token = fb_OAuth,
          feed = TRUE, since = paste(year, '/01/01', sep = ''),
          until = paste(year, '/12/31', sep = ''), n = as.numeric(nposts))
  
  #option in function
  #Function to get pages
}

# Facebook data gathering loop
fb_pages <- c(facebook_names)
for (vendor in facebook_names) {
  for (year in years) {
    svar <- paste(vendor, year, sep = '_')
    #combine vender/year , seperate by _
    nposts <- 12000
    #number of posts
    tk <- tryCatch(fbGetPages(vendor, svar, year, nposts), error = function(e) { cat('In error handler\n'); print(e); e })
    assign(svar, tk)
    #Try this, if it errors, catch and throw error as to not halt process
    save(list = svar, file = paste(svar, 'RData', sep = '.'))
    #combin vender/year , seperate by _
    Sys.sleep(2)
    remove('tk')
  }
}

# Twitter loop to retrieve userTimeline
for(name in twitter_names) {
  tmp <- paste(name, 'timeline', sep = '_')
  dat <- userTimeline(name, n = 3200)
  assign(tmp, dat)
  save(list = tmp, file = paste(tmp, 'RData', sep = '.'))
  Sys.sleep(2)
  remove(tmp, dat)
}

# Twitter loop to retrieve getUser info
for(name in twitter_names) {
  tmp <- paste(substring(name, 1), 'user', sep = '_')
  dat <- getUser(name)
  assign(tmp, dat)
  save(list = tmp, file = paste(tmp, 'RData', sep = '.'))
  Sys.sleep(2)
  remove(tmp, dat)
}

# Save all data objects
filenames <- list.files(path = 'raw_data', pattern = '*.RData', full.names = TRUE)
lapply(filenames, load, .GlobalEnv)
remove(filenames)

# Create Facebook object aggregates
labatt <- rbind(labattusa_2013, labattusa_2014, labattusa_2015, labattusa_2016)
molson <- rbind(molsoncanadianusa_2013, molsoncanadianusa_2014, molsoncanadianusa_2015, molsoncanadianusa_2016)
ultra <- rbind(MichelobULTRA_2011, MichelobULTRA_2012, MichelobULTRA_2013,
             MichelobULTRA_2014, MichelobULTRA_2015, MichelobULTRA_2016)
bud <- rbind(BudLight_2011, BudLight_2012, BudLight_2013, BudLight_2014, BudLight_2015, BudLight_2016)

# Save objects
save(labatt, file = 'processed_data/labatt_fb.RData')
save(molson, file = 'processed_data/molson_fb.RData')
save(ultra, file = 'processed_data/ultra_fb.RData')
save(bud, file = 'processed_data/bud_fb.RData')

# Create Twitter objects
LabattUSA_timeline <- twListToDF(LabattUSA_timeline)
Molson_Canadian_timeline <- twListToDF(Molson_Canadian_timeline)
MichelobULTRA_timeline <- twListToDF(MichelobULTRA_timeline)
budlight_timeline <- twListToDF(budlight_timeline)

LabattUSA_user <- LabattUSA_user
Molson_Canadian <- Molson_Canadian_user
ultra_user <- MichelobULTRA_user
bud_user <- budlight_user

# Save timeline objects
save(LabattUSA_timeline, file = 'processed_data/labattusa_timeline.RData')
save(Molson_Canadian_timeline, file = 'processed_data/Molson_Canadian_timeline.RData')
save(MichelobULTRA_timeline, file = 'processed_data/MichelobULTRA_timeline.RData')
save(budlight_timeline, file = 'processed_data/budlight_timeline.RData')

save(LabattUSA_user, file = 'processed_data/labattusa_user.RData')
save(Molson_Canadian_user, file = 'processed_data/Molson_Canadian_user.RData')
save(MichelobULTRA_user, file = 'processed_data/MichelobULTRA_user.RData')
save(budlight_user, file = 'processed_data/budlight_user.RData')

# Create Facebook Summary Object
Company <- c('Labatt USA', 'Molson Canadian', 'Michelob ULTRA', 'Bud Light')
Comments <- c(sum(labatt$comments_count), sum(molson$comments_count), sum(ultra$comments_count), sum(bud$comments_count))
Likes <- c(sum(labatt$likes_count), sum(molson$likes_count), sum(ultra$likes_count), sum(bud$likes_count))
Shares <- c(sum(labatt$shares_count), sum(molson$shares_count), sum(ultra$shares_count), sum(bud$shares_count))
Total.Posts <- c(1315, 517, 3484, 6927)
summary_stats <- data.frame(Company, Comments, Likes, Shares, Total.Posts)
all_companies <- rbind(labatt, molson, ultra, bud)

# Save Summary Object
save(summary_stats, file = 'processed_data/summary_stats.RData')
save(all_companies, file = 'processed_data/all_companies.RData')

# Search Twitter
#tmp <- searchTwitter("senior care", n=1000, geocode='40.1253890991,-74.9998931885,30mi')
#tmp2 <- searchTwitter("senior living", n=1000, geocode='40.1253890991,-74.9998931885,30mi')
#tmp3 <- searchTwitter("retirement home", n=1000, geocode='40.1253890991,-74.9998931885,30mi')
#tmp4 <- searchTwitter("assisted living", n=1000, geocode='40.1253890991,-74.9998931885,30mi')




# END OF GATHERING