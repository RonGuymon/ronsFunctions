#' @title Get Facebook Ad Details with Breakdowns
#' @description This function is useful to get details of ads. Returns a lot more data because it's broken down.
#'
#' This function requires that you set up an app on Facebook, and that you have access to the appropriate adAccount. Customers should give you access to your personal Facebook account. They can do it through business.facebook.com.
#' Video about what the api does: https://www.facebook.com/marketingdevelopers/videos/772240782890323/
#' Video about using the marketing api: https://www.facebook.com/marketingdevelopers/videos/772232066224528/
#' Great explanation of  how to get started, and on which I found the syntax it on github: https://github.com/cardcorp/fbRads
#' To get an api key, you need to get the appId, appSecret, and adAccount number.
#' Once you get those three things, you can get the access token in the following way:
#' fb_app <- oauth_app(appname = "facebook", key = appId, secret = appSecret) # Define the app
#' creds <- oauth2.0_token(oauth_endpoints("facebook"), fb_app, scope = 'public_profile'
#'                      , type = 'application/x-www-form-urlencoded', cache = T)
#' accessToken <- creds$credentials %>% names () %>% fromJSON() %>% .$access_token
#' Breakdowns: https://developers.facebook.com/docs/marketing-api/insights/parameters
#'
#'
#' Depends on tidyverse, lubridate, magrittr, httr, jsonlite.
#' @param adAccount The quoted adAccount id.
#' @param accessToken The quoted access token. Keep this safe and secure.
#' @param sinceDate Quoted beginning date of when to aggregate data in the format of "2018-11-09".
#' @param untilDate Quoted end date of when to aggregate data in the format of "2018-11-09".
#' @param level Defaults to 'ad'
#' @param breakdowns Defaults to age,gender. Can also be one or more of the following: country, dma, frequency_value, hourly_stats_aggregated_by_advertiser_time_zone, hourly_stats_aggregated_by_audience_time_zone, impression_device, place_page_id, publisher_platform, platform_position, device_platform, product_id, region, ad_format_asset, body_asset, call_to_action_asset, description_asset, image_asset, link_url_asset, title_asset, video_asset
#' @param limit Defaults to 10000. Can be any other integer. Larger numbers may cause a rate filter to kick in, which seems to last for about 10 minutes.
#' @param fields Defaults to ad_id,objective,cpc,cpp,ctr,impressions,spend. There are many others.
#' @param verbose Defaults to TRUE, and will report the details of the call.
#' @return Returns a dataframe with a row for each ad/breakdown/date combination. All columns are character.
#' @export
faceGetAdBreakdown <- function(adAccount, accessToken, sinceDate, untilDate
                               , level = 'ad'
                               , breakdowns = 'age,gender'
                               , limit = 10000
                               , fields = 'ad_id,objective,cpc,cpp,ctr,impressions,spend'
                               , verbose = T
                               ){
  if(verbose == T){
    r <- httr::GET(paste0("https://graph.facebook.com/v3.2/act_"
                          , adAccount
                          , "/insights?"
                          , "level=",level,"&"
                          , 'time_range={"since":"', sinceDate, '","until":"', untilDate, '"}&'
                          , 'breakdowns=',breakdowns,'&' #hourly_stats_aggregated_by_audience_time_zone   age,gender publisher_platform
                          , 'limit=',limit,'&'
                          , 'fields=',fields,'&'
                          , "access_token=", accessToken)
                   , verbose()
    )
  }else{
    r <- httr::GET(paste0("https://graph.facebook.com/v3.2/act_"
                          , adAccount
                          , "/insights?"
                          , "level=",level,"&"
                          , 'time_range={"since":"', sinceDate, '","until":"', untilDate, '"}&'
                          , 'breakdowns=',breakdowns,'&' #hourly_stats_aggregated_by_audience_time_zone   age,gender publisher_platform
                          , 'limit=',limit,'&'
                          , 'fields=',fields,'&'
                          , "access_token=", accessToken)
                   # , verbose()
    )
  }
  df <- content(r, as = "text") %>%
    fromJSON() %>%
    .$data
  return(df)

}
