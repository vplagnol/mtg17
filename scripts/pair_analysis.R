library(dplyr)

set <- 'Aetherdrift'
iFile <- paste0("processed_data/game_data_", set, "_reduced_GIH.csv")

if (!exists("card_data")) card_data <- readr::read_csv(iFile)



card1 = 'Wail of War'; card2 = 'Shocking Sharpshooter'
card1 = 'Delta Bloodflies'; card2 = 'Snakeskin Veil'




all_card_names =  gsub(grepv(names(card_data), pattern = "GIH_"), pattern = "GIH_", replacement = "")
pairs_matrix <- combn(all_card_names, 2)


## proceed by chunk of 1K
if (!file.exists("temp"))  dir.create("temp")



chunk<- 1
start <- 1000*(chunk-1) + 1
end <- 1000*chunk


while (end < ncol(pairs_matrix)) {

    oFile <- paste0("temp/", set, "/pairs_chunk_", chunk, ".csv")
    while (file.exists(oFile)) {
        chunk = chunk + 1
        oFile <- paste0("temp/", set, "/pairs_chunk_", chunk, ".csv")
        start <- 1000*(chunk-1) + 1
        end <- 1000*chunk
    }
    message("Processing chunk ", chunk, " from ", start, " to ", end)

    all_data = list()
    for (i in start:end) {
        print(i)
        card1 = pairs_matrix[1,i]
        card2 = pairs_matrix[2,i]
        
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
            pair_analysis = dplyr::bind_rows(all_data) %>% dplyr::arrange(desc(interaction_term))
            interesting <- dplyr::filter(pair_analysis, interaction_pval < 0.05, interaction_term > 0)
            write.csv(interesting, file = paste0('temp/interesting_', set, '_paired_analysis_temp.csv'))
        }

    }

    
    ## let's wrap things up
    pair_analysis = dplyr::bind_rows(all_data)
    write.csv(pair_analysis, file = oFile)

    chunk <- chunk + 1
    start <- 1000*(chunk-1) + 1
    end <- 1000*chunk

}


