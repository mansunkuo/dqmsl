#!/usr/bin/env Rscript
library(rvest)
library(stringr)
library(RCurl)
library(data.table)

ranking_pages = read_html("http://dqmsl-search.net/ranking/allsbjstatus?hide=&hides=,star1,star2,star3,star4")
urls_xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb']/div/div[@class='innnerHideDiv']/div/div/a"
base_url = "http://dqmsl-search.net"
monster_urls = ranking_pages %>% 
    html_nodes(xpath = urls_xpath) %>%
    html_attr(name = "href") %>%
    unique() %>%
    (function(x) {paste0(base_url, x, sep = "")}) 

monsters = data.table()
for (monster_url in monster_urls) {
    monster_page = read_html(monster_url)
    id = str_extract(monster_url, "[0-9]*$")
    
    name = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxh']/h1") %>%
        html_text() %>%
        (function(x) {gsub("-.*-$", "", x)})
    
    icon_url = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxh']/img[@id='msIcn']") %>%
        html_attr(name = "src") 
    download.file(sprintf("%s%s", base_url, icon_url), 
                  gsub("^\\/", "", icon_url))
    
    mrank = monster_page %>%
        html_nodes(xpath = "//span[@id='maxChartId']") %>%
        html_text() %>%
        str_replace_all("\n|\r|\t|ランク最高スコア表示", "")
    
    msystem = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][1]/div[3]/div/span[@class='enclose3'][3]/a[@class='linkTextF']") %>%
        html_text() %>%
        str_replace_all("\n|\r|\t|系統：", "")
    
    mtype = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][1]/div[3]/div/span[@class='enclose3'][4]/a[@class='linkTextF']") %>%
        html_text() %>%
        str_replace_all("\n|\r|\t|タイプ：", "")
    
    mweight = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][1]/div[3]/div/span[@class='enclose3'][5]/a[@class='linkTextF']") %>%
        html_text() %>%
        str_replace_all("\n|\r|\t|ｳｪｲﾄ：", "")
    
    mmaxlevel = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][1]/div[3]/div/span[@class='enclose3'][6]") %>%
        html_text() %>%
        str_replace_all("\n|\r|\t|最大レベル：", "")
    
    mhp = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][1]/span[@id='hpid']") %>%
        html_text()
    
    mmp = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][2]/span[@id='mpid']") %>%
        html_text()
    
    mstr = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][3]/span[@id='ofid']") %>%
        html_text()
    
    mdef = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][4]/span[@id='dfid']") %>%
        html_text()
    
    magi = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][5]/span[@id='dxid']") %>%
        html_text()
    
    mint = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxb'][2]/div[*]/div/div[1]/span[@class='enclose3'][6]/span[@id='eiid']") %>%
        html_text()
    
    view = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='lhbox']/span[@class='lhnum']") %>%
        html_text() %>%
        str_replace_all(",", "")
    
    like = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='lhbox']/div/span[@class='lhlbox']/span[@id='lhnuml']") %>%
        html_text()
    
    hate = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='lhbox']/div/span[@class='lhhbox']/span[@id='lhnumh']") %>%
        html_text()
    
    monster = data.table(id = id,
                         name = name,
                         rank = mrank,
                         system = msystem,
                         type = mtype,
                         weight = mweight,
                         maxlevel = mmaxlevel,
                         hp = mhp,
                         mp = mmp,
                         str = mstr,
                         def = mdef,
                         agi = magi,
                         int = mint,
                         view = view,
                         like = like,
                         hate = hate)        
    monsters = rbind(monsters, monster)
    Sys.sleep(rpois(1, 1))
}

write.csv(monsters, "data/monsters.csv", row.names = FALSE)
