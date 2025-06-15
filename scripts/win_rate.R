


##data <- readr::read_csv("processed_data/game_data_public.TDM.PremierDraft_small.csv")

all_card_names =  gsub(grepv(names(data), pattern = "drawn_"), pattern = "drawn_", replacement = "")

all_data = list()

for (card in all_card_names) {
    print(card)
    
    mytab <- table(data[[paste0('drawn_', card)]], data[['won']])
    fish = fisher.test(mytab[c(1,2),])
    
    all_data[[ card ]] = list(card = card, OR = fish$estimate, pvalue = fish$p.value, GIH_WR =  mytab["1",]['TRUE'] / sum(mytab["1",]))
    
}



final_table = dplyr::bind_rows(all_data)
final_table =  final_table[ order(final_table$OR, decreasing = TRUE),]
