library(XML)
library(dplyr)
library(assertthat)

url <- "http://turistautak.hu/poi.php?lap=1&egylapon=500&submit_egylapon=OK&dist_lat=&dist_lon=&poi_any=&poi_nickname=&poi_fulldesc=&poi_placer=&poi_city=&poi_member=&poi_alt_min=&poi_alt_max=&poi_dateposted_min=&poi_dateposted_max=&poi_dist_min=&poi_dist_max=&code[]=41477&login_user_id=42001&no_caches=i&action=browse"

#Check
raw_page <- htmlTreeParse(url,
                          useInternalNodes = TRUE,
                          encoding = "UTF8")
parsed <- xpathSApply(doc = raw_page,
                      "//b",
                      xmlValue)
number_of_records_to_get <- as.numeric(strsplit(x = grep(pattern = "db",
                                                          x = parsed,
                                                          value = TRUE),
                                                split = " ")[[1]][1])

#Find pages
tables <- readHTMLTable(url,
                        encoding = "UTF8",
                        stringsAsFactors = FALSE)
pages_info <- as.character(tables[4])
pages <- unlist(lapply(X = strsplit(x = pages_info,
                                    split = "\\|"),
            FUN = as.numeric))
from_page <- min(pages, na.rm = TRUE)
to_page <- max(pages, na.rm = TRUE)


poi_table <- NULL
for (i in from_page:to_page) {
  url <- paste("http://turistautak.hu/poi.php?lap=",
  i,
  "&egylapon=500&submit_egylapon=OK&dist_lat=&dist_lon=&poi_any=",
  "&poi_nickname=&poi_fulldesc=&poi_placer=&poi_city=&poi_member=",
  "&poi_alt_min=&poi_alt_max=&poi_dateposted_min=&poi_dateposted_max=",
  "&poi_dist_min=&poi_dist_max=&code[]=41477&login_user_id=",
  "42001&no_caches=i&action=browse",
  sep = "")
  tables <- readHTMLTable(url,
                          encoding = "UTF8")#,
#                          stringsAsFactors = FALSE)
  tmp_table <- tbl_df(tables[[3]])
  colnames(tmp_table) <- c("jel", "nev", "telepules", "megye", "bejelentes",
                           "magassag", "felhasznalo", "koo", "tav", "fenykep")
  tmp_table <- tmp_table %>%
    filter(!is.na(nev)) %>%
    filter(nev != "Neve") %>%
    select(-1, -9)
  poi_table <- rbind(poi_table,
                     tmp_table)
}

#Check that really everything has been downloaded
assert_that(number_of_records_to_get == dim(poi_table)[1])

pois %>% group_by(felhasznalo) %>% summarise(darab = n()) %>% arrange(desc(darab))

pois %>% group_by(megye) %>% summarise(darab = n()) %>% arrange(desc(darab))