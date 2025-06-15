stop()

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
