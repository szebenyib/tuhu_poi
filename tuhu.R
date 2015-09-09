library(XML)
library(dplyr)
library(tidyr)
library(assertthat)
library(lubridate)
library(stringr)

#Symbol to use
geocaching_user_id <- "42001"
sym <- "tourism-drinkingwater"
url <- paste("http://turistautak.hu/poi.php?lap=1&egylapon=500",
             "&submit_egylapon=OK&dist_lat=&dist_lon=&poi_any=",
             "&poi_nickname=&poi_fulldesc=&poi_placer=&poi_city=&poi_member=",
             "&poi_alt_min=&poi_alt_max=&poi_dateposted_min=",
             "&poi_dateposted_max=&poi_dist_min=&",
             "poi_dist_max=&code[]=41477&login_user_id=",
             geocaching_user_id,
             "&no_caches=i&action=browse",
             sep = "")

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
  geocaching_user_id,
  "&no_caches=i&action=browse",
  sep = "")
  tables <- readHTMLTable(url,
                          encoding = "UTF8",
                          stringsAsFactors = FALSE,
                          na.strings = c("", " ", "NA", "&nbsp;"))
  tmp_table <- tbl_df(tables[[3]])
  colnames(tmp_table) <- c("jel", "nev", "telepules", "megye", "bejelentes",
                           "magassag", "felhasznalo", "koo", "tav", "fenykep")
  tmp_table <- tmp_table %>%
    filter(!is.na(nev)) %>%
    filter(nev != "Neve") %>%
    select(-jel, -tav)
  raw_page <- htmlTreeParse(url,
                            useInternalNodes = TRUE,
                            encoding = "UTF8")
  poi_link <- xpathSApply(doc = raw_page,
                          "//a[@class='nal a10']",
                          xmlGetAttr, 'href')
  tmp_table <- cbind(tmp_table,
                     poi_link)
  poi_table <- rbind(poi_table,
                     tmp_table)
}
#Remove leftover
rm(tables)
#Check that really everything has been downloaded
assert_that(number_of_records_to_get == dim(poi_table)[1])

pois %>% group_by(felhasznalo) %>% summarise(darab = n()) %>% arrange(desc(darab))

pois %>% group_by(megye) %>% summarise(darab = n()) %>% arrange(desc(darab))

#Remove invalid
poi_table <- poi_table %>%
  filter(koo != "N 00° 0,000'\nE 00° 0,000'")

#Temp storage to avoid redownload of data
poi_bak <- poi_table
#Name space trimming
poi_table <- poi_table %>%
  mutate(nev = substr(x = nev,
                      start = 1,
                      stop = str_length(nev) - 1))
#Coordinate transformation
poi_table <- poi_table %>%
  separate(col = koo,
           into = c("lat", "lon"),
           sep = "\n") %>%
  separate(col = lat,
           into = c("lat_deg", "lat_dec"),
           sep = " ") %>%
  separate(col = lon,
           into = c("lon_deg", "lon_dec"),
           sep = " ") %>%
  mutate(lat_deg = as.numeric(substr(x = lat_deg,
                                     start = 1,
                                     stop = 2))) %>%
  mutate(lon_deg = as.numeric(substr(x = lon_deg,
                                     start = 1,
                                     stop = 2))) %>%
  mutate(lat_dec = substr(x = lat_dec,
                          start = 1,
                          stop = 5)) %>%
  mutate(lat_dec = as.numeric(sub(pattern = ",",
                                  replacement = ".",
                                  lat_dec))) %>%
  mutate(lat_dec = lat_dec / 60) %>%
  mutate(lon_dec = substr(x = lon_dec,
                          start = 1,
                          stop = 5)) %>%
  mutate(lon_dec = as.numeric(sub(pattern = ",",
                                  replacement = ".",
                                  lon_dec))) %>%
  mutate(lon_dec = lon_dec / 60) %>%
  mutate(lat = lat_deg + lat_dec) %>%
  mutate(lon = lon_deg + lon_dec) %>%
  mutate(lat = round(lat,
                     digits = 4)) %>%
  mutate(lon = round(lon,
                     digits = 4)) %>%
  select(-lat_deg, -lat_dec, -lon_deg, -lon_dec)
#Photos are filled with zero if they are empty
poi_table <- poi_table %>%
  mutate(fenykep = as.numeric(fenykep))
  
header <- paste("<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n",
  "<gpx version=\"1.0\" creator=\"Locus Android\"\n",
  "  xmlns=\"http://www.topografix.com/GPX/1/0\"\n",
  "  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n",
  "  xmlns:locus=\"http://www.locusmap.eu\"\n",
  "  xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0\n",
  "  http://www.topografix.com/GPX/1/0/gpx.xsd\">\n",
  "<metadata>\n",
  "  <desc>Grabbed from turistautak.hu</desc>\n",
  "</metadata>",
  sep = "")
footer <- paste("</gpx>")
utc_stamp <- with_tz(time = now(),
                     tzone = "UTC")
utc_stamp <- floor_date(x = utc_stamp,
                        unit = "second")
utc_stamp <- paste(year(utc_stamp),"-",
                   sprintf("%02d", month(utc_stamp)), "-", 
                   sprintf("%02d", day(utc_stamp)), "T",
                   sprintf("%02d", hour(utc_stamp)), ":",
                   sprintf("%02d", minute(utc_stamp)), ":",
                   sprintf("%02d", second(utc_stamp)), "Z",
                   sep = "")
content <- NULL
for (i in 1:dim(poi_table)[1]) {
  content <- c(content, 
               paste("<wpt lat=\"", poi_table[i, "lat"], "\" ",
                   "lon=\"", poi_table[i, "lon"], "\">\n",
                   #                 "  <ele>96.00</ele>\n",
                   "  <time>", utc_stamp, "</time>\n",
                   "  <name><![CDATA[", poi_table[i, "nev"], "]]></name>\n",
                   "  <desc><![CDATA[Fényképek: ", poi_table[i, "fenykep"], "]]></desc>\n",
                   "  <url>http://www.turistautak.hu", poi_table[i, "poi_link"], "</url>\n",
                   "  <sym>", sym, "</sym>\n",
                   "</wpt>",
                   sep = ""))  
}

#Output contents to poi.gpx.
write(x = header,
      file = "poi.gpx",
      append = FALSE)
write(x = content,
      file = "poi.gpx",
      append = TRUE)
write(x = footer,
      file = "poi.gpx",
      append = TRUE)