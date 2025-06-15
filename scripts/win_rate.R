


##data <- readr::read_csv("processed_data/game_data_public.TDM.PremierDraft_small.csv")

all_card_names =  gsub(grepv(names(data), pattern = "drawn_"), pattern = "drawn_", replacement = "")


estimate_single_card_wr = FALSE

if (estimate_single_card_wr) {
    all_data = list()
    
    for (card in all_card_names) {
        print(card)
        
        mytab <- table(data[[paste0('drawn_', card)]], data[['won']])
        fish = fisher.test(mytab[c(1,2),])
        
        all_data[[ card ]] = list(card = card, OR = fish$estimate, pvalue = fish$p.value, GIH_WR =  mytab["1",]['TRUE'] / sum(mytab["1",]))
        
    }
    
    single_final_table = dplyr::bind_rows(all_data)
    single_final_table =  final_table[ order(final_table$OR, decreasing = TRUE),]
}



card1 = 'Wail of War'; card2 = 'Shocking Sharpshooter'


all_data = list()

pairs_matrix <- combn(all_card_names, 2)

for (i in 1:ncol(pairs_matrix)) {
    print(i)
    card1 = pairs_matrix[1,i]
    card2 = pairs_matrix[2,i]
    
    #print(single_final_table[ single_final_table$card == card1,])
    #print(single_final_table[ single_final_table$card == card2,])

    both_drawn = data[[paste0('drawn_', card1)]] >= 1 & data[[paste0('drawn_', card2)]] >= 1
    count_pairs = sum( both_drawn )

    if (count_pairs > 100) {
        print(card1)
        print(card2)
        
        card1OR =  single_final_table[ single_final_table$card == card1,]$OR
        card2OR =  single_final_table[ single_final_table$card == card2,]$OR
        combined_OR = card1OR*card2OR
        
        mytab = fisher.test(table(both_drawn, data$won))
        all_data[[ paste(card1, card2, sep = "_") ]] = list(card1 = card1,
                                                            card2 = card2,
                                                            count = count_pairs,
                                                            card1OR = card1OR,
                                                            card2OR = card2OR,
                                                            low_CI = mytab$conf.int[1],
                                                            high_CI = mytab$conf.int[2],
                                                            theory_OR = combined_OR,
                                                            observed_OR = mytab$estimate,
                                                            flag = mytab$conf.int[1] > combined_OR)
    }
}

pair_analysis = dplyr::bind_rows(all_data)
pair_analysis_flag = dplyr::filter(pair_analysis, flag)
pair_analysis_flag =  pair_analysis_flag[ order(pair_analysis_flag$observed_OR, decreasing = TRUE),]
