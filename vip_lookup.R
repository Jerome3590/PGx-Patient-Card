
library(rvest)
library(V8)


#URL with js-rendered content to be scraped
link <- 'https://www.pharmgkb.org/vips'

#Read the html page content and extract all javascript codes that are inside a list
vip_js <- read_html(link) %>% html_nodes('li') %>% html_nodes('script') %>% html_text()

# Create a new v8 context
ct <- v8()

#parse the html content from the js output and print it as text
read_html(ct$eval(gsub('document.write','',vip_js))) %>% 
  html_text()



