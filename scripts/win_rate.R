


if (!exists("card_data")) card_data <- readr::read_csv("processed_data/game_data_public.TDM.PremierDraft_small.csv")

all_card_names =  gsub(grepv(names(card_data), pattern = "drawn_"), pattern = "drawn_", replacement = "")

ref_color = names(sort(table(card_data$main_colors), decreasing = TRUE))
card_data$main_colors = factor(card_data$main_colors, levels = ref_color)

print(summary(glm(data = card_data, formula = 'won~ main_colors', family = binomial)))





estimate_single_card_wr = TRUE

if (estimate_single_card_wr) {
    all_data = list()
    
    for (card in all_card_names) {
        print(card)

        my_label = paste0('drawn_', card)
        
        mytab <- table(card_data[[paste0('drawn_', card)]], data[['won']])
        fish = fisher.test(mytab[c(1,2),])
        
        all_data[[ card ]] = list(card = card,
                                  OR = fish$estimate,
                                  pvalue = fish$p.value,
                                  GIH_WR =  mytab["1",]['TRUE'] / sum(mytab["1",]),
                                  U = cor.test(card_data[[ my_label]], data$deck_Island)$estimate,
                                  B = cor.test(card_data[[ my_label]], data$deck_Swamp)$estimate,
                                  R = cor.test(card_data[[ my_label]], data$deck_Mountain)$estimate,
                                  W = cor.test(card_data[[ my_label]], data$deck_Plains)$estimate,
                                  G = cor.test(card_data[[ my_label]], data$deck_Forest)$estimate)
                                  
        
    }
    
    single_final_table = dplyr::bind_rows(all_data)
    single_final_table =  single_final_table[ order(final_table$OR, decreasing = TRUE),]
}


