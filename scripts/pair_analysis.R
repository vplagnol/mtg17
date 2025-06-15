
if (!exists("card_data")) card_data <- readr::read_csv("processed_data/game_data_public.TDM.PremierDraft_small_GIH.csv")

card1 = 'Wail of War'; card2 = 'Shocking Sharpshooter'
card1 = 'Delta Bloodflies'; card2 = 'Snakeskin Veil'


all_data = list()

pairs_matrix <- combn(all_card_names, 2)

for (i in 1:ncol(pairs_matrix)) {
    print(i)
    card1 = pairs_matrix[1,i]
    card2 = pairs_matrix[2,i]
    
    #print(single_final_table[ single_final_table$card == card1,])
    #print(single_final_table[ single_final_table$card == card2,])

    both_GIH = card_data[[paste0('GIH_', card1)]] >= 1 & card_data[[paste0('GIH_', card2)]] >= 1
    count_pairs = sum( both_GIH )

    if (count_pairs > 100) {
        print(card1)
        print(card2)


        mod <- glm(data = card_data, formula = paste0('won ~ `GIH_', card1, '` * `GIH_', card2, '`'), family = binomial)
        summary(mod)
        interaction_pval = as.numeric(coef(summary(mod))[4, 'Pr(>|z|)'])
        interaction_term = as.numeric(coef(summary(mod))[4, 'Estimate'])
        
        all_data[[ paste(card1, card2, sep = "_") ]] = list(card1 = card1,
                                                            card2 = card2,
                                                            count = count_pairs,
                                                            interaction_term = interaction_term,
                                                            interaction_pval = interaction_pval)

        if (interaction_pval < 0.05) print(summary(mod))
    }
    if (i %% 100 == 99) {
        pair_analysis = dplyr::bind_rows(all_data)
        pair_analysis = pair_analysis[ order(pair_analysis$interaction_pval, decreasing = FALSE), ]
        write.csv(pair_analysis, file = 'processed_data/paired_analysis.csv')
    }
}


