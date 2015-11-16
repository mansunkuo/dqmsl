#!/usr/bin/env Rscript
library(rvest)
library(stringr)
library(RCurl)
library(data.table)
library(XML)

ranking_pages = read_html("http://dqmsl-search.net/ranking/allsbjstatus?hide=&hides=,star1,star2,star3,star4")
urls_xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb']/div/div[@class='innnerHideDiv']/div/div/a"
base_url = "http://dqmsl-search.net"
monster_urls = ranking_pages %>% 
    html_nodes(xpath = urls_xpath) %>%
    html_attr(name = "href") %>%
    unique() %>%
    (function(x) {paste0(base_url, x, sep = "")}) 

# download html files
dir_name = "html"
dir.create(dir_name, showWarnings = FALSE)
file_names = str_replace_all(monster_urls, ".*\\?", "") %>%
    (function(x){paste(dir_name, "/", x, ".html", sep = "")})
# for (i in 1:length(monster_urls)) {
#     message(i)
#     download.file(monster_urls[i], file_names[i])
#     Sys.sleep(rpois(1, 1)+1)
#     
#     monster_page = read_html(file_names[i])
#     icon_url = monster_page %>%
#         html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxh']/img[@id='msIcn']") %>%
#         html_attr(name = "src") 
#     download.file(sprintf("%s%s", base_url, icon_url), 
#                   gsub("^\\/", "", icon_url))
#     Sys.sleep(rpois(1, 1)+1)
# }


monsters = data.table()
# for (monster_url in monster_urls) {
# for (monster_url in monster_urls[1]) {
file_names = list.files("html", pattern = ".*no.*", full.names = TRUE)
for (i in 1:length(file_names)) {
    # monster_page = read_html(monster_url)
    print(i)
    monster_page = read_html(file_names[i])
    id = str_extract(file_names[i], "[0-9]+")
    
    name = monster_page %>%
        html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxh']/h1") %>%
        html_text() %>%
        (function(x) {gsub("-.*-$", "", x)})
    
#     icon_url = monster_page %>%
#         html_nodes(xpath = "/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][1]/div[@class='mboxh']/img[@id='msIcn']") %>%
#         html_attr(name = "src") 
#     download.file(sprintf("%s%s", base_url, icon_url), 
#                   gsub("^\\/", "", icon_url))
    
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
}

write.csv(monsters, "data/monsters.csv", row.names = FALSE)


# skills
skill_file = paste(dir_name, "/skills.html", sep = "")
download.file("http://dqmsl-search.net/picturebook/skill", skill_file)
skill_pages = read_html(skill_file)
skills = data.table()
for (i in 1:6) {
    skill_types = skill_pages %>%
        html_nodes(xpath = sprintf("/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb'][%s]/div[*]/span[@class='srchead']/span", i)) %>%
        html_text()
    print(skill_types)
    for (j in 1:length(skill_types)) {
        print(sprintf("##########%s##########", skill_types[j]))
        if (i == 1) {
            skill_vec = skill_pages %>%
                html_nodes(xpath = sprintf("/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb'][1]/div[2]/div[@class='innnerHideDiv1']/div[*]/span[@class='srchead']/span")) %>%
                html_text()
        } else {
            skill_vec = skill_pages %>%
                html_nodes(xpath = sprintf("/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb'][%s]/div[%s]/div[@class='innnerHideDiv1']/div[*]/span[@class='srchead']/span", i, j)) %>%
                html_text()
        }
        
        print(skill_vec)
        for (k in 1:length(skill_vec)) {
            print(sprintf("##########%s##########", skill_vec[k]))
            if (i ==1) {
                monsters = skill_pages %>%
                    html_nodes(xpath = sprintf("/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb'][1]/div[2]/div[@class='innnerHideDiv1']/div[%s]/div[@class='innnerHideDiv2']/div[2]/div[@class='innnerHideDiv4']/div[*]/span", k)) %>%
                    html_text()
            } else {
                monsters = skill_pages %>%
                    html_nodes(xpath = sprintf("/html/body/div[@class='mainh']/div[@class='mainc']/div[@class='ccol']/div[@class='mbox'][2]/div[@class='mboxb'][%s]/div[%s]/div[@class='innnerHideDiv1']/div[%s]/div[@class='innnerHideDiv2']/div[2]/div[@class='innnerHideDiv4']/div[*]/span", i, j, k)) %>%
                    html_text()
            }
            print(monsters)
            if (length(monsters > 0)) {
                temp_dt = data.table(skill_type = skill_types[j],
                                     skill = skill_vec[k],
                                     monster = monsters)
                skills = rbind(skills, temp_dt)
            }
        }
    }
}

write.csv(skills, "data/skills.csv", row.names = FALSE)
