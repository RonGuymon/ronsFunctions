#' @title Get Shopify Data on an Hourly Basis
#' @description This function requires a Shopify Key, Password, and path.
#'
#' The Shopify API key requires the store owner to go in and create it. I need to document how.
#'
#'
#'
#' Depends on base64enc, httr, lubridate, tidyverse, magrittr, jsonlite.
#' @param shopKey The quoted api key. Get this  and shopPw by creating a private app. Must use the customer's login credentials, to do so.
#' @param shopPw The quoted api password.
#' @param shopPath The quoted path. The path is something like "myStoreName.com"
#' @param hourSequence Vector of timestamps ordered from most recent to oldest. Doesn't have to be in hourly increments.
#' @param consecutiveZeros Integer representing the number of consecutive times in which no data is returned before stopping.
#' @return A dataframe with a row for each item in the transaction. Some columns are summary for the whole order.
#' @export

shopifyHourly <- function(shopKey, shopPw, shopPath, hourSequence, consecutiveZeros){
  # Format apiKey
  shopifyApiKey <- paste(shopKey, shopPw, sep = ":") %>% charToRaw() %>% base64encode()

  # Function: Collapses line items to a single row
  lif <- function(x){
    temp <- x %>% as.data.frame(stringsAsFactors = F) %>%
      dplyr::select(title) %>% # grams, variant_title, quantity
      mutate(item_category = gsub(" \\|.*$", "", title) %>% gsub("\\|.*$", "", .) %>% gsub("^.* ", "", .)) %>%
      apply(., 2, function(x) paste(x, collapse = ", ")) %>%
      t() %>% as.data.frame(stringsAsFactors = F)
  }

  # Function: Collapses discounts to a single row
  ld <- function(x){
    temp <- x %>% as.data.frame(stringsAsFactors = F)
    if(nrow(temp) == 0){
      temp <- data.frame(order_discount_code = NA,
                         order_discount_amount = NA,
                         order_discount_type = NA,
                         stringsAsFactors = F)
    }else{
      temp %<>%
        apply(., 2, function(x) paste(x, collapse = ", ")) %>%
        t() %>% as.data.frame(stringsAsFactors = F)
    }
  }


  no_data_indicator <- 0 # Keep track of the number of time periods without any data
  dataTime <- Sys.time()
  allOrders <- data.frame()
  for(hour_n in 1:(length(hourSequence)-1)){

    r <- GET(paste0("https://", shopPath, "/admin/orders.json?limit=250&created_at_min=",hourSequence[hour_n+1],"MST-07:00&created_at_max=",hourSequence[hour_n],"MST-7:00&status=any"),
             add_headers("Authorization" = paste0("Basic ", shopifyApiKey)
                         , "Content-Type" = "application/json"
             )
             # ,verbose()
    )

    allo <- content(r, as = "text") %>% fromJSON() %>% as.data.frame()
    norders <- nrow(allo)
    # Move to next if there aren't any orders during that time
    if(norders == 0){
      no_data_indicator <- no_data_indicator + 1
      cat("No data for", hour_n, hourSequence[hour_n], "Consecutive no data periods:", no_data_indicator, "\n")
      if(hour_n %% 2 == 0){
        Sys.sleep(1)
      }
      if(no_data_indicator >= 100){
        break()
      }else{
        next()}
    }else{
      no_data_indicator <- 0
    }

    colnames(allo) <- gsub("orders\\.", "", colnames(allo))
    allo$user_agent <- allo$client_details$user_agent
    allo$total_spent <- allo$customer$total_spent

    # Refund amount (negative number)
    allo$refundAmount <- 0
    for(k in 1:nrow(allo)){
      if(!is.null(allo$refunds[[k]]$order_adjustments[[1]]$amount)){
        allo$refundAmount[k] <- allo$refunds[[k]]$order_adjustments[[1]]$amount %>% as.numeric()
      }
    }

    tryCatch({
      ba <- allo$billing_address %>%
        dplyr::select(city, zip, province_code, country_code, latitude, longitude)
      colnames(ba) <- paste0("billing_", colnames(ba))
    }, error = function(e){
      ba <<- data.frame()
      cat("No Billing address information")
    })
    tryCatch({
      sa <- allo$shipping_address %>%
        dplyr::select(city, zip, province_code, country_code, latitude, longitude)
      colnames(sa) <- paste0("shipping_", colnames(sa))
    }, error = function(e){
      sa <<- data.frame()
      cat("No Shipping address information.")
    })

    # Collapse line items and categories to a single row
    all_items <- lapply(allo$line_items, lif) %>% rbindlist()
    if(ncol(all_items) > 0){
      colnames(all_items) <- c("all_order_titles", "all_order_categories")
    }

    # Grab and collapse discount codes, amounts, and types to a single row
    all_discounts <- lapply(allo$discount_codes, ld) %>% rbindlist()
    # colnames(all_discounts) <- paste0("order_discount_", colnames(all_discounts))

    # Creates a new row for each line item. When left joined, it adds a new row for each line item
    for(j in 1:nrow(allo)){
      temp <- allo$line_items[j] %>% as.data.frame(stringsAsFactors = F) %>%
        dplyr::select(product_id, title, quantity, price, grams, variant_title)
      temp$item_number <- seq(1, nrow(temp), by = 1)
      temp$order_quantity <- sum(temp$quantity, na.rm = T)
      temp$order_grams <- sum(temp$grams, na.rm = T)
      temp$order_items <- nrow(temp)
      temp$header = ifelse(temp$item_number == 1, 1, 0) # Dummy to indicate if it will be used as the summary for the order
      temp$id <- allo$id[j]
      # temp$header == 0

      # tempd <- allo$discount_codes[j] %>% as.data.frame(stringsAsFactors = F)
      # tempd$id <- allo$id[j]
      if(j == 1){
        lineItems <- temp
        # discounts <- tempd
      }else{
        lineItems %<>% bind_rows(., temp)
        # discounts %<>% bind_rows(.,tempd)
      }
    }

    varsToKeep <- c("id", "email", "contact_email", "created_at", "number", "gateway", "total_line_items_price",
                    "total_weight", "financial_status", "fulfillment_status", "referring_site", "landing_site",
                    "processing_method", "test", "total_spent", "refundAmount")
    allo <- allo[,which(colnames(allo) %in% varsToKeep)] %>%
      dplyr::filter(test == F) # Removes rows that are test transactions
    allo %<>% bind_cols(., ba)
    allo %<>% bind_cols(., sa)
    allo %<>% bind_cols(., all_items)
    allo %<>% bind_cols(., all_discounts)
    allo %<>% left_join(., lineItems, by = "id")

    allo %<>%
      mutate(
        fulfillment_status = ifelse(is.na(fulfillment_status), "not fulfilled", fulfillment_status)
      )


    allOrders %<>% bind_rows(., allo)
    cat("Done with ", hour_n, hourSequence[hour_n], ". # of orders = ", norders, "\n")
  }

  if(nrow(allOrders > 0)){
    allOrders <- allOrders %>%
      mutate(
        created_at = ymd_hms(created_at)  %>% with_tz(tzone = "US/Mountain")
        , total_line_items_price = as.numeric(total_line_items_price)
        , order_discount_amount = ifelse("order_discount_amount" %in% colnames(allOrders),as.numeric(order_discount_amount), 0)
        , price = as.numeric(price)
        , dataTime = dataTime
        , email = tolower(email)
      )
    colsToRemove <- c("order_discount_order_discount_code", "order_discount_order_discount_amount",
                      "order_discount_order_discount_type")

    allOrders <- allOrders[,which(!colnames(allOrders) %in% colsToRemove)] %>%
      dplyr::rename(OrderID = id, BillToEmail = email, OrderDate = created_at, shopify_number = number,
                    OrderStatus = fulfillment_status, OrderTotal = total_line_items_price,
                    ShipToCity = shipping_city, ShipToPostalCode = shipping_zip, ShipToState = shipping_province_code,
                    ShipToCountry = shipping_country_code, ItemName_all = all_order_titles, ItemCategory_all = all_order_categories,
                    ItemSKU = product_id, ItemName = title, ItemQuantity = quantity, ItemUnitPrice = price, Options = variant_title,
                    ItemNumber = item_number, ItemQuantity_all = order_quantity, UniqueItems = order_items
      ) %>%
      mutate(
        OrderID = paste0("Shopify_", OrderID)
        , OrderNumber = OrderID
        , ItemCategory = gsub(" \\|.*$", "", ItemName) %>% gsub("\\|.*$", "", .) %>% gsub("^.* ", "", .) %>% gsub("[^A-Za-z0-9]", "", .) %>% tolower() %>% gsub("s$", "", .)
        , lineRev = ItemUnitPrice*ItemQuantity
        , ItemSKU = as.character(ItemSKU)
        , ItemNumber = as.character(ItemNumber)
        , line_id = OrderID, ItemNumber, OrderDate
        , Source = "Shopify"
      ) %>%
      dplyr::filter(test == F) %>%
      dplyr::select(-test, -header)


  }else{
    cat("No new data to index")
  }
  return(allOrders)

}
