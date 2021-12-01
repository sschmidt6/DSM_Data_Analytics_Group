library(rvest)
library(dplyr)

link = "https://www.meetup.com/Des-Moines-Data-Analytics/members/"
page = read_html(link)

name = page %>% html_nodes(.text--ellipsisOneLine) %>% html_text()
joined = page %>% html_nodes(.text--ellipsisOneLine) %>% html_text()
