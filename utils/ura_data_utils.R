require(jsonlite)
require(curl)
require(clock)

get_rental_data <- function(ystart, yend,
                            mstart, mend,
                            dtoken){
  ## Function to fetch available data with input date range
  # ystart - start of date range (year; 4-digit integer)
  # yend - end of date range (year; 4-digit integer)
  # mstart - start of date range (month; [1-12])
  # mend - end of date range (month; [1-12])
  # t - path to ura token
  # dtoken - dropbox auth token (environment variable)
  
  start_date <- date_build(ystart,mstart,1)
  end_date <- date_build(yend, mend, 1)
  range_date <- date_seq(start_date, to = end_date, by = duration_months(1)) %>%
    as_year_month_day()
  range_quarter <- range_date %>% as_year_quarter_day() %>%
    calendar_group("quarter") %>% unique()
  q <- to_ura_quarter(range_quarter)
  if(length(q) == 0){
    stop("no data for provided date range")
  }
  ura_df <- lapply(q, function(x){
    drop_path <- sprintf("ura_rental/data/%s", x)
    file_info <- drop_dir(drop_path, dtoken = dtoken)
    file_info <- file_info %>%
      mutate(local_file = file.exists(paste0("data/", name)))
    tmp <- lapply(1:nrow(file_info),
                  function(y){
                    if(file_info$local_file[y]){
                      read_csv(paste0("data/", file_info$name[y])) %>%
                        mutate(noOfBedRoom = as.character(noOfBedRoom)) %>%
                        mutate(x = as.numeric(x)) %>%
                        mutate(y = as.numeric(y))
                    } else {
                      drop_download(file_info$path_display[y],
                                    local_path = "data/", dtoken = dtoken)
                      read_csv(paste0("data/", file_info$name[y])) %>%
                        mutate(noOfBedRoom = as.character(noOfBedRoom)) %>%
                        mutate(x = as.numeric(x)) %>%
                        mutate(y = as.numeric(y))
                    }
                  })
    tmp <- bind_rows(tmp)
    })
  ura_df <- bind_rows(ura_df) %>%
    mutate(noOfBedRoom = gsub("00","0", noOfBedRoom))
  return(bind_rows(ura_df))
}

get_condo_list <- function(dtoken){
  drop_download("ura_rental/data/condos.rds", local_path = "data/", dtoken = dtoken, overwrite = T)
  condos <- readRDS("data/condos.rds")
  return(condos)
}

retrieve_last_update_file <- function(dtoken){
  drop_download("ura_rental/data/last_update.csv", local_path = ".", dtoken=dtoken, overwrite=T)
}

is_updated <- function(y, m, dtoken){
  ## Function to check if update is already performed
  # load last_update.csv file
  drop_path <- "ura_rental/data"
  drop_download(sprintf("%s/last_update.csv", drop_path), local_path=".", dtoken=dtoken, overwrite = T)
  last_update <- read_csv("last_update.csv")
  last_y <- as.integer(last_update$y[1])
  last_m <- as.integer(last_update$m[1])
  now_q <- year_month_day(y,m,1) %>% as_year_quarter_day() %>% get_quarter()
  last_q <- year_month_day(last_y,last_m,1) %>% as_year_quarter_day() %>% get_quarter()
  if(year_month_day(y,m,1) > year_month_day(last_y, last_m, 1)){
    return(FALSE)
  } else {
    return(TRUE)
  }
  
  
  #drop_path <- sprintf("ura_rental/data/%s", q)
  #file_info <- drop_dir(drop_path, dtoken = dtoken)
  #if(nrow(file_info) == 0){
  #  return(FALSE)
  #} else {
  #  file_info <- file_info %>%
  #    filter(name == sprintf("%s-%s.csv",y,m))
  #  return(nrow(file_info == 1))
  #}
}

update_ura_data <- function(y, m, dtoken){
  ## Function to update collection of URA data for given year and month
  ## URA data is updated on the 15th of every month
  target_date <- year_month_day(y,m,1)
  q <- parseDate(target_date) %>% to_ura_quarter()
  uratoken <- get_token(dtoken = dtoken)
  ura_data <- get_ura_data(q, uratoken$Result)
  ura_df <- process_ura_data(ura_data)
  ura_df <- ura_df %>%
    filter(leaseDate == add_months(target_date,0))
  # update condo list
  update_condo_list(unique(ura_df$project), dtoken)
  drop_path <- sprintf("ura_rental/data/%s", q)
  tryCatch(drop_dir(drop_path), 
           error = function(cond){
             sprintf("%s does not exist, creating one now...", drop_path)
             drop_create(drop_path, dtoken=dtoken)
           })
  ura_file <- sprintf("data/%s-%s.csv", y, m)
  write_csv(ura_df, ura_file)
  drop_upload(ura_file, path = drop_path, dtoken = dtoken)
}

update_condo_list <- function(new_condo, dtoken){
  condos <- get_condo_list(dtoken)
  condos <- unique(c(condos, new_condo))
  saveRDS(condos, "data/condos.rds")
  drop_upload("data/condos.rds", "ura_rental/data/", dtoken = dtoken)
}

get_ura_data <- function(q,
                         uratoken){
  
  h <- new_handle()
  handle_setheaders(h,
                    "AccessKey" = "8da3d5f8-fa26-421a-95c5-58d2095cff84", 
                    "Token" = uratoken)
  ura_data <- curl(url = paste0("https://eservice.ura.gov.sg/uraDataService/invokeUraDS/v1?service=PMI_Resi_Rental&refPeriod=",q),
                   handle = h)
  return(readLines(ura_data))
}

process_ura_data <- function(ura_data){
  ura_data <- parse_json(ura_data, simplifyVector = T)
  ura_df <- as_tibble(ura_data$Result) %>%
    unnest(cols = c(rental)) %>%
    mutate_at(vars(starts_with("area")),
              list("mid" = ~ binArea(.x))) %>%
    mutate(leaseDate = parseDate(leaseDate, precision = "day"),
      quarter = parseDate(leaseDate, precision = "quarter"),
      rent_psf = rent/areaSqft_mid)
  return(ura_df)
}


binArea <- function(x){
  results <- sapply(x,
                    function(y){
                      if(length(grep("-",y))){
                        temp <- unlist(strsplit(y,"-"))
                        temp <- as.numeric(temp)
                        temp <- (temp[1] + temp[2])/2
                        } else {
                          temp = gsub("[^0-9]","",y)
                          temp = as.numeric(temp)
                        }
                    })
  return(results)
}

parseDate <- function(x, precision = "quarter"){
  if(!is_year_month_day(x)){
    results <- year_month_day_parse(paste0("01",x), format = "%d%m%y")
  } else {
    results <- x
  }
  if(precision == "quarter"){
    results <- as_year_quarter_day(results) %>%
      calendar_group(precision = precision)
  } else {
    results <- calendar_group(results, precision = precision)
  }
  return(results)
}

to_ura_quarter <- function(x){
  yy <- get_year(x) %>% substr(3,4)
  qq <- get_quarter(x)
  return(sprintf("%sq%s",yy,qq))
}  
  
  