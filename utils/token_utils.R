require(curl)
require(jsonlite)
require(rdrop2)

get_token <- function(t = "tmp/token.txt",
                      dtoken){
  token_path <- paste0("ura_rental/", t)
  if(drop_exists(token_path, dtoken = dtoken)){
    #print("token exists")
    f <- drop_dir(dirname(token_path), dtoken = dtoken)
    if(difftime(Sys.time(), f$server_modified, unit = "days") < 1){
      #print("token new")
      drop_download(token_path, t, overwrite = T, dtoken = dtoken)
      return(read_json(t))
      }
    }
  h <- new_handle()
  handle_setheaders(h, "AccessKey" = "8da3d5f8-fa26-421a-95c5-58d2095cff84")
  tok <- curl(url = "https://eservice.ura.gov.sg/uraDataService/insertNewToken/v1", handle = h)
  tok <- readLines(tok)
  write(tok, file = t)
  drop_upload(file = t, 
              path = "ura_rental/tmp",
              dtoken = dtoken)
  return(get_token(t, dtoken))
  }
  

