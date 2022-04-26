
#Calling Libraries

library(curl)
library(rjson)
library(jsonlite)
library(httr)


# 
# req <- curl_fetch_memory("https://www.bing.com/covid/data/")
# str(req)
# 
# 
# a <- jsonlite::prettify(rawToChar(req$content))


bcovid <- paste0('https://www.bing.com/covid/data/')


covid <- httr::GET(bcovid)


my_content <- httr::content(covid,as = 'text')



my_content_from_json <- jsonlite::fromJSON(my_content)



test <- my_content_from_json$areas



map_df(test, function(x) {
  
  df <- flatten_df(x[c("id", "areas")]) 
  
  map_df(x$mentions, ~c(as.list(.$text), mentions_type=.$type)) %>%
    mutate(name=df$name, type=df$type, mid=df$mid,
           wikipedia_url=df$wikipedia_url, salience=df$salience)
  
}) %>% glimpse()






test_us <- test[test$id == 'unitedstates',]


data1 <- fromJSON("https://www.bing.com/covid/data/")


testus <- as.data.frame(test_us$areas)




test_ny <- testus[testus$id == 'newyork_unitedstates',]

testus <- as.data.frame(test_ny$areas) 
