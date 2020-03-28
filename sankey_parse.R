library(jsonlite)

Usersfile <- ('assets/sankey_per_site.json')
json <- do.call(rbind, 
                lapply(paste(readLines(Usersfile, warn=FALSE),
                             collapse=""), 
                       jsonlite::fromJSON))

main_sample = jsonlite::stream_in(file("assets/sankey_per_site.json"),pagesize = 100000)
