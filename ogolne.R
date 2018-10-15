
pacman::p_load(dplyr,
               magrittr,
               rvest,
               pdftools,
               stringr)

egzamin_raw <- read_html("http://www.ekonometria.wne.uw.edu.pl/index.php?n=Main.Egzaminy") 

ogolne = egzamin_raw %>%
  html_nodes(xpath="//*[@id='wikitext']/ul/li/ul/li[1]/a") %>%
  html_attr('href') 
ogolne = ogolne[grepl('01\\.|02\\.',ogolne)]

teoria=list()
for(i in 1:length(ogolne)){
  teoria = c(teoria,gsub('\r\n','',gsub('\\s{1,10}',' ',pdftools::pdf_text(ogolne[i])[1])))
}

quest = lapply(stringr::str_extract_all(teoria,'[0-9]\\.\\s.+[\\.\\?]\\s'),function(x) x)
quest_clean = unlist(lapply(unlist(quest),function(i) trimws(substr(i,4,1000))))
map_d = cbind(c('ª','»','±','¦','¢','˛','¡'),c('ł','ż','ś','ę','ć','ó','ą'))
replace_strange = function(x){for(i in 1:nrow(map_d)){x<-gsub(map_d[i,1],map_d[i,2],x)};x}
quest_clean = replace_strange(quest_clean)
result = trimws(unlist(strsplit(quest_clean,'\\d\\.')))
table(result)
