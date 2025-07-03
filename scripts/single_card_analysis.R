library(dplyr)

set <- 'Tarkir'

iFile <- paste0("processed_data/game_data_", set, "_reduced.csv")
intermediate_file <- paste0("processed_data/game_data_", set, "_reduced_GIH.csv")


if (!exists("card_data")) {
    card_data <- readr::read_csv(iFile)
    message("Done reading")
    all_card_names =  gsub(grepv(names(card_data), pattern = "drawn_"), pattern = "drawn_", replacement = "")  ## list all card names

    for (card in all_card_names) {
        card_data [[ paste0('GIH_', card) ]] <- card_data [[ paste0('drawn_', card) ]] + card_data [[ paste0('opening_hand_', card) ]]
    }
    card_data <- card_data[ , -  grep(pattern = "^opening", x = names(card_data)) ]
    card_data <- card_data[ , -  grep(pattern = "^drawn", x = names(card_data)) ]

    write.csv(card_data, file = intermediate_file)
}



## sanity check
sanity <- FALSE
if (sanity) {
    test <- dplyr::filter(card_data, main_colors == 'BG')
    #test <- dplyr::filter(card_data, main_colors == 'BG' & is.na(splash_colors))
    card <- 'Bloomvine Regent'
    print(mean(test$won))
    print(fisher.test(table(test$won, test$`GIH_Bloomvine Regent` >= 1)))
    test2 <- dplyr::filter(test, `GIH_Bloomvine Regent` >= 1)
    print(mean(test2$won))

    test2 <- dplyr::filter(test, `GIH_Bloomvine Regent` == 1)
    print(mean(test2$won))
    
    #print(fisher.test(table(card_data$won, card_data$`GIH_Bloomvine Regent` >= 1 & card_data$main_colors == 'BG')))
    stop()

    ##unweighted_model_s <- glm(data = test, formula = paste0('won ~ `GIH_', card, '`'), family = binomial)  # s stands for specific here


    
}



all_card_names =  gsub(grepv(names(card_data), pattern = "GIH_"), pattern = "GIH_", replacement = "")



ref_color = names(sort(table(card_data$main_colors), decreasing = TRUE))
card_data$main_colors = factor(card_data$main_colors, levels = ref_color) #use as ref the most common color combo

print(summary(glm(data = card_data, formula = 'won~ main_colors', family = binomial)))


color_names <- function(c) {
    if (c == 'WR') return ('Boros')
    if (c == 'WG') return ('Selesnya')
    if (c == 'WU') return ('Azorius')
    if (c == 'WB') return ('Orzhov')
    
    if (c == 'BG') return ('Golgari')
    if (c == 'BR') return ('Rakdos')
    if (c == 'UB') return ('Dimir')
    
    if (c == 'UG') return ('Simic')
    if (c == 'UR') return ('Izzet')

    if (c == 'RG') return ('Gruul')

    return(c)
}


card_value_per_deck <- list()

card_data$archetype <- dplyr::if_else(is.na(card_data$splash_colors),
                                      card_data$main_colors,
                                      paste0(card_data$main_colors, ' splash: ', card_data$splash_colors))
colour_table <- table(card_data$archetype)
covered_archetypes <-  names(colour_table[ which(colour_table > 3000) ])


archetype_info <- list()
for (my_archetype in covered_archetypes) {
    card_value_per_deck[[ my_archetype ]] <- list()
    averageWR <- mean(card_data$won[ card_data$archetype == my_archetype])
    archetype_info [[ my_archetype ]] <- list(archetype = my_archetype,
                                           averageWR = averageWR,
                                           n_decks = sum(card_data$archetype == my_archetype))
}

archetype_info <- dplyr::bind_rows(archetype_info) %>% dplyr::arrange(desc(averageWR))

mean_win_rate <-  mean(card_data$won)
mean_odds_win = mean_win_rate/(1-mean_win_rate)

all_data = list()

#all_card_names <- c('Bloomvine Regent')
for (card in all_card_names) {
    print(card)
    total_count <- sum(card_data[my_label ])
    
    my_label = paste0('GIH_', card)
    
    mytab <- table(card_data[[paste0('GIH_', card)]], card_data[['won']])
    fish = fisher.test(mytab[c(1,2),])
 
    unweighted_model <- glm(data = card_data, formula = paste0('won ~ `GIH_', card, '`'), family = binomial)
    #weighted_model <- glm(data = card_data, formula = paste0('won ~ `GIH_', card, '`'), family = binomial, weights = 1/(7 + card_data$num_turns))
    weighted_model <- unweighted_model  ## for the sake of speed
    
    odds_weighted <- mean_odds_win*exp(coef(weighted_model)[2])
    odds_unweighted <- mean_odds_win*exp(coef(unweighted_model)[2])

    GIH_win_rate_modelled_weighted <- odds_weighted/(1 + odds_weighted)
    GIH_win_rate_modelled_unweighted <- odds_unweighted/(1 + odds_unweighted)

    ## generic card analysis, not colour specific
    local_res = list(card = card,
                     total_count = total_count,
                     OR = as.numeric(fish$estimate),
                     GIH_WR_basic =  as.numeric(mytab["1",]['TRUE'] / sum(mytab["1",])),
                     GIH_win_rate_modelled_weighted = GIH_win_rate_modelled_weighted,
                     GIH_win_rate_modelled_unweighted = GIH_win_rate_modelled_unweighted)
    all_data[[ card ]] <- local_res

    ### now we move to the archetype specific analysis
    card_colour_table <- table(card_data$archetype[ card_data[[ my_label ]]  >= 1 ] ) ## colours that card is played in
    relevant_archetypes <- intersect(names(card_colour_table[ card_colour_table > 200 ]), covered_archetypes) ## intersect these colours with the generally well covered decks
    
    for (my_archetype in relevant_archetypes) { ## loop over the well covered colours
        message("Archetyoe, ", my_archetype)
        n_decks_with_card_in_hand <- card_colour_table[ my_archetype ]
        relevant_decks <- dplyr::filter(card_data,  archetype == my_archetype)
        unweighted_model_s <- glm(data = relevant_decks, formula = paste0('won ~ `GIH_', card, '`'), family = binomial)  # s stands for specific here
        odds_ratio_unweighted_s <- exp(coef(unweighted_model_s)[2])
        GIH_WR <- mean(relevant_decks$won[ relevant_decks[ my_label ] >= 1 ])

        local_res_s <- list(card = card,
                            archetype = my_archetype,
                            OR = odds_ratio_unweighted_s,
                            GIH_WR = GIH_WR,
                            n_decks_with_card_in_hand = n_decks_with_card_in_hand)
        
        card_value_per_deck[[ my_archetype ]][[ card ]] <- local_res_s
    }    
}

## clean up the tables
first_color <- TRUE
oFile <- paste0('processed_data/for_draft/', set, '/per_archetype_', set, '.csv')
for (my_archetype in covered_archetypes) {
    cat("Archetype:,", color_names(my_archetype), ", win rate: ", archetype_info$averageWR[ archetype_info$archetype == my_archetype ], "\n", file = oFile, append = !first_color)
    first_color = FALSE
    
    card_value_per_deck[[ my_archetype ]] <-  dplyr::arrange(dplyr::bind_rows( card_value_per_deck[[ my_archetype ]]), desc(OR))
    readr::write_csv( card_value_per_deck[[ my_archetype ]], file = oFile, append = TRUE, col_names = TRUE)
    cat("\n\n\n\n", file = oFile, append = TRUE)
}
print(oFile)

single_final_table = dplyr::bind_rows(all_data)
single_final_table =  single_final_table[ order(single_final_table$GIH_win_rate_modelled_weighted, decreasing = TRUE),]


single_final_table <- dplyr::filter(single_final_table,
                                   ! card %in% c("Mountain", "Plains", "Island", "Forest", "Swamp"))

write.csv(single_final_table, file = paste0('processed_data/single_card_analysis_', set, '.csv'))



#interesting_cards <- dplyr::mutate(single_final_table, ratio = GIH_WR_color_matched/GIH_WR_basic) %>%
#    dplyr::arrange(desc(ratio)) %>%
#    dplyr::select(card, color, GIH_WR_basic, GIH_WR_color_matched, nb_games_GIH_color_matched)


print(interesting_cards)






