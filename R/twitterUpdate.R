#' @title Daily Twitter Data update
#' @description Uses the twitteR package to get witter data for: Timeline tweets, followers, following, hashtags, and mentions.
#'
#' You need to authenticate before using this function:setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)
#'
#' Run the function the same way regardless of whether you are updating your existing data, or if you are starting fresh.
#' @param screenName The screen name of the twitter account, without the hashtag. For example, RonGuymon or RickGuymon
#' @param directory Full directory path where the data will be stored. For example, /Volumes/GreatCustomer/Data/. If the directory doesn't already exist, a new one will be created.
#' @return Returns a list with an update of what was written to file.
#' @export
#'
twitterUpdate <- function(screenName, directory){
  startTime <- Sys.time()
  statusList <- list()
  dir.create(directory, showWarnings = F) # Creates a directory if it doesn't already exist. If the directory already exists, then nothing happens.
  filesInDirectory <- list.files(directory) # Get a list of files in the directory
  dataTime <- Sys.time() # To keep track of when following/followers were added

  ####### USER DESCRIPTION #######
  cat("Getting User description data for ", screenName, "\n")
  me = getUser(screenName)
  userDescription <- data.frame(
    screenName  = screenName
    , profileName = me$name
    , profileId = me$id
    , profileCreated = me$created
    , statusesCount = me$statusesCount
    , description = me$description
    , followersCount = me$followersCount
    , followingCount = me$friendsCount
    , likesCount = me$favoritesCount
    , loke = me$location
    , lastStatusText = me$lastStatus$text
    , lastStatusCreated = me$lastStatus$created
    # , profileImageUrl = me$profileImageUrl
    , dataTime = Sys.time() %>% with_tz("America/Denver")
    , stringsAsFactors = F
  )
  tudFileName <- "twitterUserDescription.csv"
  tudFileNameFull <- paste0(directory, tudFileName)

  if(tudFileName %in% filesInDirectory){
    udOld <- read.csv2(tudFileNameFull, header = T, sep = ",", stringsAsFactors = F) %>%
      dplyr::mutate(
        profileId = as.character(profileId)
        , profileCreated = ymd_hms(profileCreated)
        , lastStatusCreated = ymd_hms(lastStatusCreated)
        , dataTime = ymd_hms(dataTime)
      )
    userDescription <- bind_rows(udOld, userDescription)
  }
  userDescription %<>% unique()

  write_csv(userDescription, tudFileNameFull) # Write to disk
  statusList$userDescription <- paste0("Wrote ", nrow(userDescription), " rows of user description data to file.")
  # rm(userDescription, udOld)
  ####### TWEETS FROM TIMELINE #######
  cat("Getting tweets from timeline for ", screenName, "\n")
  tttFileName <- "twitterTimelineTweets.csv"
  tttFileNameFull <- paste0(directory, tttFileName)
  if(tttFileName %in% filesInDirectory){
    timelineTweets <- read.csv2(tttFileNameFull, header = T, sep = ",", stringsAsFactors = F) %>% # Read in existing file with old tweets
      dplyr::mutate(
        created = ymd_hms(created)
        , id = as.character(id)
        , replyToSID = as.character(replyToSID)
        , replyToUID = as.character(replyToUID)
      )

    # Only get the most recent 500 tweets
    ttNew <- userTimeline(screenName, n=500, excludeReplies = F)

    if(length(ttNew) > 0){
      ttNew <- twListToDF(ttNew)

      ttNew2 <- ttNew %>%
        dplyr::mutate(
          newData = "yes"
          , myid  = paste0(text, as.character(created))
        ) %>%
        dplyr::select(myid, newData)

      # Add the new tweets to the old ones and remove the old ones if they exist so that we have the most recent count of engagement
      timelineTweets %<>%
        dplyr::mutate(
          myid  = paste0(text, as.character(created))
        ) %>%
        left_join(., ttNew2, by = "myid") %>%
        dplyr::filter(is.na(newData)) %>%
        dplyr::select(-newData, -myid) %>%
        bind_rows(., ttNew) %>%
        .[!duplicated(.$id),]
    }



  }else{
    timelineTweets <- userTimeline(screenName, n=3200, excludeReplies = F) %>%
      twListToDF()
    maxID <- timelineTweets$id[nrow(timelineTweets)]
    for(i in 1:25){
      cat(i, "######### GETTING TWEETS BEFORE", maxID, "\n")
      tryCatch({
        temp <- userTimeline(screenName, n = 3200, maxID = maxID, excludeReplies = F)
        if(length(temp) > 0){
          temp <- twListToDF(temp)
          maxID <- temp$id[nrow(temp)]
          timelineTweets %<>% bind_rows(., temp)
          cat("Tweets Retrieved:", nrow(temp), "Cum Tweets: ", nrow(timelineTweets), "\n")
        }
      }, error = function(e){
        cat("Problem with getting timeline tweets at ", i, "\n")
      })
    }
    timelineTweets %<>% unique()
  }

  write_csv(timelineTweets, tttFileNameFull) # Write to disk
  statusList$timelineTweets <- paste0("Wrote ", nrow(timelineTweets), " rows of timeline tweet data to file.")

  # rm(ttNew, ttNew2, timelineTweets)
  ####### FOLLOWERS #######
  cat("Getting Followers for ", screenName, "\n")
  tFollowersFileName <- "twitterFollowers.csv"
  tFollowersFileNameFull <- paste0(directory, tFollowersFileName)
  if(tFollowersFileName %in% filesInDirectory){
    followers <- read.csv2(tFollowersFileNameFull, header = T, sep = ",", stringsAsFactors = F, fill = F) %>% # Read in old followers
      dplyr::mutate(
        created = ymd_hms(created)
        , id = as.character(id)
        , followerDate = ymd_hms(followerDate)
      )
    # Get latest list of followers from api
    nFollowers <- me$getFollowers()
    if(length(nFollowers) > 0){
      nFollowers <- twListToDF(nFollowers) %>%
        dplyr::left_join(., followers[,c("id", "followerDate")], by = "id") %>%
        dplyr::rename(followerDateOrig = followerDate)

      # Prepare a column to join to keep the followers stats up to date
      nFollowers2 <- nFollowers %>%
        dplyr::mutate(
          newData = "yes"
        ) %>%
        dplyr::select(screenName, newData)

      # Update the table
      followers %<>%
        left_join(., nFollowers2, by = "screenName") %>%
        dplyr::filter(is.na(newData)) %>% # Only keep the new followers indicated by not having new data
        bind_rows(., nFollowers)
      if(!"followerDate" %in% colnames(followers)){
        followers$followingDate <- NA
      }
      followers %<>%
        dplyr::mutate(
          followerDate = case_when(
            is.na(followerDateOrig) & !is.na(followerDate) ~ followerDate
            , is.na(followerDateOrig) & is.na(followerDate) ~ dataTime
            , T ~ followerDateOrig
          )
        ) %>%
        dplyr::select(-followerDateOrig, -newData) %>%
        .[!duplicated(.$id),]
    }
  }else{
    followers <- me$getFollowers() %>%
      twListToDF() %>%
      dplyr::mutate(
        followerDate = dataTime
      )
  }
  write_csv(followers, tFollowersFileNameFull) # Write to disk
  statusList$followers <- paste0("Wrote ", nrow(followers), " rows of follower data to file.")

  # rm(nFollowers2, nFollowers)

  ###### FOLLOWING #######
  cat("Getting Following for ", screenName, "\n")
  tFollowingFileName <- "twitterFollowing.csv"
  tFollowingFileNameFull <- paste0(directory, tFollowingFileName)
  if(tFollowingFileName %in% filesInDirectory){
    following <- read.csv2(tFollowingFileNameFull, header = T, sep = ",", stringsAsFactors = F) %>%
      dplyr::mutate(
        id = as.character(id)
        , created = ymd_hms(created)
        , followingDate = ymd_hms(followingDate)
      )

    # Get latest list of following from api
    nFollowing <- me$getFriends()

    if(length(nFollowing) > 0){
      nFollowing <- twListToDF(nFollowing) %>%
        dplyr::left_join(., following[,c("id", "followingDate")], by = "id") %>%
        dplyr::rename(followingDateOrig = followingDate)

      # Prepare a column to join to keep the followers stats up to date
      nFollowing2 <- nFollowing %>%
        dplyr::mutate(
          newData = "yes"
        ) %>%
        dplyr::select(id, newData)

      # Update the table
      following %<>%
        left_join(., nFollowing2, by = "id") %>%
        dplyr::filter(is.na(newData)) %>%
        bind_rows(., nFollowing)
      if(!"followingDate" %in% colnames(following)){
        following$followingDate <- NA
      }
      following %<>%
        dplyr::mutate(
          followingDate = case_when(
            is.na(followingDateOrig) & !is.na(followingDate) ~ followingDate
            , is.na(followingDateOrig) & is.na(followingDate) ~ dataTime
            , T ~ followingDateOrig
          )
        ) %>%
        dplyr::select(-followingDateOrig, -newData) %>%
        .[!duplicated(.$id),]
    }
  }else{
    following <- me$getFriends() %>%
      twListToDF() %>%
      dplyr::mutate(
        followingDate = dataTime
      )
  }
  write_csv(following, tFollowingFileNameFull) # Write to disk
  statusList$following <- paste0("Wrote ", nrow(following), " rows of following data to file.")
  # rm(following, nFollowing, nFollowing2)
  ####### HASHTAGS ########
  cat("Getting hashtags for ", screenName, "\n")
  thFileName <- "twitterHashtags.csv"
  thFileNameFull <- paste0(directory, thFileName)
  if(thFileName %in% filesInDirectory){
    hashies <- read.csv2(thFileNameFull, header = T, sep = ",", stringsAsFactors = F) %>%  # Read in old data
      dplyr::mutate(
        id = as.character(id)
        , created = ymd_hms(created)
        , replyToUID = as.character(replyToUID)
        , replyToSID = as.character(replyToSID)
        , myid = paste0(text, as.character(created))
      )

    # Collect new hashtags
    hNew <- searchTwitter(paste0("#", screenName), n = 500)
    if(length(hashies) > 0){
      hNew <- hNew %>%
        twListToDF() %>%
        dplyr::mutate(
          myid = paste0(text, as.character(created))
          )

      hNew2 <- hNew %>%
        dplyr::mutate(
          new = "yes"
        ) %>%
        dplyr::select(myid, new)

      hashies %<>%
        left_join(., hNew2, by = "myid") %>%
        dplyr::filter(is.na(new)) %>%
        dplyr::select(-new) %>%
        bind_rows(., hNew) %>%
        .[!duplicated(.$id),] %>%
        dplyr::select(-myid)
    }
  }else{
    hashies <- searchTwitter(paste0("#", screenName), n = 500, since = "2012-01-01")
    if(length(hashies) > 0){
      hashies <- hashies %>%
        twListToDF()
      maxID <- hashies$id[nrow(hashies)]
      for(i in 1:50){
        cat("####### STARTING ", i, maxID, "########", "\n")
        temp <- searchTwitter(paste0("#", screenName), n = 500, maxID = maxID) %>%
          twListToDF()
        maxID <- temp$id[nrow(temp)]
        hashies %<>% bind_rows(., temp)
        cat("Done with ", i, maxID, "Cum Rows: ", nrow(hashies), "\n")
      }
      hashies %<>% unique()
    }

  }
  if(!is.null(nrow(hashies))){
    write_csv(hashies, thFileNameFull) # Write to disk
    statusList$hastags <- paste0("Wrote ", nrow(hashies), " rows of hashtag data to file.")
  }else{
    statusList$hastags <- paste0("Wrote 0 rows of hashtag data to file.")
  }
  # rm(hashies, hNew, hNew2)
  ####### MENTIONS ########
  cat("Getting mentions for ", screenName, "\n")
  tmFileName <- "twitterMentions.csv"
  tmFileNameFull <- paste0(directory, tmFileName)
  if(tmFileName %in% filesInDirectory){
    mentions <- read.csv2(tmFileNameFull, header = T, sep = ",", stringsAsFactors = F) %>%
      dplyr::mutate(
        id = as.character(id)
        , created = ymd_hms(created)
        , replyToUID = as.character(replyToUID)
        , replyToSID = as.character(replyToSID)
        , myid = paste0(text, as.character(created))
      )

    mNew <- searchTwitter(paste0("@", screenName), n = 500)
    if(length(mNew) > 0){
      mNew <- mNew %>%
        twListToDF() %>%
        dplyr::mutate(
          myid = paste0(text, as.character(created))
          )

      mNew2 <- mNew %>%
        dplyr::mutate(
          new = "yes"
        ) %>%
        dplyr::select(myid, new)

      mentions %<>%
        left_join(., mNew2, by = "myid") %>%
        dplyr::filter(is.na(new)) %>%
        dplyr::select(-new) %>%
        bind_rows(., mNew) %>%
        .[!duplicated(.$myid),] %>%
        dplyr::select(-myid)
    }

  }else{
    mentions <- searchTwitter(paste0("@", screenName), n = 500)
    if(length(mentions) > 0){
      mentions <- mentions %>%
        twListToDF()

      maxID <- mentions$id[nrow(mentions)]
      for(i in 1:50){
        cat("####### STARTING ", i, maxID, "########", "\n")
        temp <- searchTwitter(paste0("@", screenName), n = 500, maxID = maxID)
        if(length(temp) > 0){
          temp <- temp %>%
            twListToDF()
          maxID <- temp$id[nrow(temp)]
          mentions %<>% bind_rows(., temp)
        }
        cat("Done with ", i, maxID, "Cum Rows: ", nrow(mentions), "\n")
      }
      mentions %<>% unique()

    }

  }
  if(!is.null(nrow(mentions))){
    write_csv(mentions, tmFileNameFull)
    statusList$mentions <- paste0("Wrote ", nrow(mentions), " rows of mention data to file.")
  }else{
    statusList$mentions <- paste0("Wrote 0 rows of mention data to file.")
  }

  cat("Finished updating twitter data for ", screenName, "\n")
  minsRequired <- difftime(Sys.time(), startTime, units = "mins") %>% as.numeric() %>% round(1)
  statusList$timeRequired <- paste0("It took ", minsRequired, " minutes to complete.")
  return(statusList)
}
