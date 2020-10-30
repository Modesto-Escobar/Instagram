# Metadata ----------------------------------------------------------------
# Title: dataviz.R
# Purpose: Dataviz instagram netcoin dataton
# Author(s): @pablocalv & @criscalopez
# Date: 2020-10-29 20:17:29
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(netCoin)


# 1. Prepare data for network ---------------------------------------------
hm <- read_csv("1_data/hm") %>% 
 pull(mentions)

d <- openxlsx::read.xlsx("1_data/data.xlsx")


# 1.1 Prepare df for coincidences -----------------------------------------
# nodes will be IG posts (need to reshape)
# links based on mentions and hashtags from a selections (institutions, politics and media)

net_df <- d %>% 
 select(code, caption_hashtags, caption_mentions) %>% 
 mutate(hashtag = str_remove_all(caption_hashtags, "\\["),
        hashtag = str_remove_all(hashtag, "\\]"),
        hashtag = str_remove_all(hashtag, "#"),
        hashtag = str_trim(hashtag, "both"),
        ment = str_remove_all(caption_mentions, "\\["),
        ment = str_remove_all(ment, "\\]"),
        ment = str_remove_all(ment, "@"),
        ment = str_trim(ment, "both")
 ) %>% 
 separate(hashtag, paste0("h_", 1:25)) %>%
 separate(ment, paste0("m_", 1:25)) %>% 
 pivot_longer(h_1:m_25, names_to = "var", values_to = "hash_men") %>% 
 filter(hash_men %in% unique(hm)) 

# coin df
coin_df <- net_df %>% 
 select(code, caption_hashtags, caption_mentions) %>% 
 mutate(hashtag = str_remove_all(caption_hashtags, "\\["),
        hashtag = str_remove_all(hashtag, "\\]"),
        hashtag = str_remove_all(hashtag, "#"),
        hashtag = str_trim(hashtag, "both"),
        ment = str_remove_all(caption_mentions, "\\["),
        ment = str_remove_all(ment, "\\]"),
        ment = str_remove_all(ment, "@"),
        ment = str_trim(ment, "both")
 ) %>% 
 separate(hashtag, paste0("h_", 1:25)) %>%
 separate(ment, paste0("m_", 1:25)) %>% 
 pivot_longer(h_1:m_25, names_to = "var", values_to = "hash_men") %>% 
 select(code, hash_men) %>%
 filter(hash_men %in% unique(hm)) %>% 
 mutate(val = 1 ) %>% 
 group_by(code, hash_men) %>% 
 summarise(val = ifelse(sum(val) > 0, 1, 0)) %>% 
 ungroup() %>% 
 pivot_wider(names_from = code, values_from = val, values_fill = 0) 


# 1.2 Auxiliary file with nodes information to plot (aes) -----------------
# node size = like_count
# node color = spoof
# label = random mention or hash
# label size = like_count
# ig post url to embed on nodes

nodes_label <- net_df %>% 
        select(code, caption_hashtags, caption_mentions) %>% 
        mutate(hashtag = str_remove_all(caption_hashtags, "\\["),
               hashtag = str_remove_all(hashtag, "\\]"),
               hashtag = str_remove_all(hashtag, "#"),
               hashtag = str_trim(hashtag, "both"),
               ment = str_remove_all(caption_mentions, "\\["),
               ment = str_remove_all(ment, "\\]"),
               ment = str_remove_all(ment, "@"),
               ment = str_trim(ment, "both")
        ) %>% 
        separate(hashtag, paste0("h_", 1:25)) %>%
        separate(ment, paste0("m_", 1:25)) %>% 
        pivot_longer(h_1:m_25, names_to = "var", values_to = "hash_men") %>% 
        select(code, hash_men) %>%  
        filter(hash_men %in% unique(hm)) %>% 
        mutate(rnd = runif(nrow(.))) %>% 
        arrange(code, -rnd) %>%  
        group_by(code) %>% 
        summarise(label = first(hash_men)) %>% 
        ungroup()

nodes_attributes <- d %>% 
        mutate(ntext = paste0('<iframe width="320" height="500" src="', url, '/embed" frameborder="0"></iframe>'),
               spoof = ifelse(spoof %in% c("UNLIKELY", "VERY_UNLIKELY"), "Neutral", "Offensive"),
               period = case_when(
                       date < lubridate::as_date("2016/01/01") ~ "1 Before politics",
                       date > lubridate::as_date("2015/12/31") & date < lubridate::as_date("2017/01/01") ~ "2 Campaign 2016",
                       date > lubridate::as_date("2016/12/31") & date < lubridate::as_date("2020/03/01") ~ "3 POTUS",
                       date > lubridate::as_date("2020/03/01")  ~ "4 COVID-19")) %>% 
        left_join(nodes_label, by = "code") %>% 
        rename(name = code) %>% 
        select(name, spoof, ntext, label, date, period, like_count)

 
# 2. Network --------------------------------------------------------------
# Prepare the network for dataviz
# one network per period and a general one

coin <- coin(coin_df[,-1])
nodes <- asNodes(coin) %>% 
 left_join(nodes_attributes, by = "name")
nodes1 <- nodes %>% 
        filter(period == "1 Before politics")
nodes2 <- nodes %>% 
        filter(period == "2 Campaign 2016")
nodes3 <- nodes %>% 
        filter(period == "3 POTUS")
nodes4 <- nodes %>% 
        filter(period == "4 COVID-19")

edges <- edgeList(coin) %>% # clear the representaiton form some links
 filter(`p(Z)` < 0.05)

nplot <- netCoin(sample_n(nodes, 400), edges, 
                 ntext = "ntext", 
                 color = "spoof", 
                 label = "label",
                 size = "like_count", 
                 main = "#MAGA @criscalopez y @pablocalv con datos de @graphext", 
                 background = "1_data/pic1.png",
                 degreeFilter = 1,
                 controls = NULL,
                 labelSize = "like_count"
                 )


nplot1 <- netCoin(nodes1, edges, 
                 ntext = "ntext", 
                 color = "spoof", 
                 label = "label",
                 size = "like_count", 
                 main = "Before politics", 
                 background = "1_data/pic1.png",
                 degreeFilter = 1,
                 controls = NULL,
                 labelSize = "like_count"
)


nplot2 <- netCoin(nodes2, edges, 
                 ntext = "ntext", 
                 color = "spoof", 
                 label = "label",
                 size = "like_count", 
                 main = "2016 Campaign", 
                 background = "1_data/pic1.png",
                 degreeFilter = 1,
                 controls = NULL,
                 labelSize = "like_count"
)

nplot3 <- netCoin(nodes3, edges, 
                 ntext = "ntext", 
                 color = "spoof", 
                 label = "label",
                 size = "like_count", 
                 main = "POTUS", 
                 background = "1_data/pic1.png",
                 degreeFilter = 1,
                 controls = NULL,
                 labelSize = "like_count"
)

nplot4 <- netCoin(nodes4, edges, 
                 ntext = "ntext", 
                 color = "spoof", 
                 label = "label",
                 size = "like_count", 
                 main = "COVID-19", 
                 background = "1_data/pic1.png",
                 degreeFilter = 1,
                 controls = NULL,
                 labelSize = "like_count"
)


multigraphCreate("#MAGA" = nplot,
                 "Before politics" =  nplot1, 
                 "2016 campaign" = nplot2, 
                 "POTUS" = nplot3, 
                 "COVID-19" = nplot4)


