library(tidyverse)
library(httr2)
library(rvest)

# get data
data = read_html("https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/Winning_Numbers/index.html_2013354932.html") %>% 
  html_nodes("table") %>%
  html_table()%>%
  as.data.frame() %>%
  select(-Jackpot.Option) %>%
  filter(!str_detect(Draw.Date, "First Drawing for Mega Millions matrix")) # remove three rows: "First Drawing for Mega Millions matrix (5 of 70 + 1 of 25) was ...."

# clean the data:
df = data %>% 
  mutate(Estimated.Jackpot.MM = unlist(
    lapply(data$Estimated.Jackpot, 
           function(x){
             ifelse(str_split(x, " ")[[1]][2] == "Billion",
                    1000*as.numeric(str_match(str_split(x, " ")[[1]][1], "\\$([0-9,.]+)")[,2]),
                    str_match(x,"\\$([0-9,.]+)")[,2])
           })
  )) %>%
  select(-Estimated.Jackpot) %>%
  separate(Draw.Date, c("month", "day", "year")) %>%
  separate(Winning.Numbers, c("first", "second", "third", "fourth", "fifth"))

# get detailed information
htmls = read_html("https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/Winning_Numbers/index.html_2013354932.html") %>%
  html_elements(".detailsLink") %>%
  html_attr("href")

base = "https://www.texaslottery.com"
detail = NULL
for (t in htmls){
  page = read_html(paste0(base, t))
  detail = rbind(
    detail,
    page %>%
      html_nodes("table") %>%
      html_table() %>%
      {.[1]} %>%
      as.data.frame() %>%
      slice(1:n()-1) %>%
      select(c(Number.Correct, Prize.Amount, Total.Texas.Winners)) %>%
      as_tibble() %>%
      nest(data = everything())
  )
}

df["detail"] = detail
df["link"] = paste0(base, htmls)
  

saveRDS(df, file="lottery.rds")
