library(dplyr)

set <- 'Tarkir'

iFile <- paste0("processed_data/game_data_", set, "_reduced.csv")
intermediate_file <- paste0("processed_data/game_data_", set, "_reduced_GIH.csv")


if (!exists("card_data")) {
    card_data <- readr::read_csv(iFile)
    message("Done reading")
    all_card_names =  gsub(grepv(names(card_data), pattern = "drawn_"), pattern = "drawn_", replacement = "")

    for (card in all_card_names) {
        card_data [[ paste0('GIH_', card) ]] <- card_data [[ paste0('drawn_', card) ]] + card_data [[ paste0('opening_hand_', card) ]]
    }
    card_data <- card_data[ , -  grep(pattern = "^opening", x = names(card_data)) ]
    card_data <- card_data[ , -  grep(pattern = "^drawn", x = names(card_data)) ]

    write.csv(card_data, file = intermediate_file)
}

all_card_names =  gsub(grepv(names(card_data), pattern = "GIH_"), pattern = "GIH_", replacement = "")



ref_color = names(sort(table(card_data$main_colors), decreasing = TRUE))
card_data$main_colors = factor(card_data$main_colors, levels = ref_color) #use as ref the most common color combo

print(summary(glm(data = card_data, formula = 'won~ main_colors', family = binomial)))



# this function estimates the baseline color of a card
f <- function(B, G, R, U, W) {
    threshold <- 0 ## works with Mox Jasper which is super flat
    res <- list()
    if (W > threshold) res["W"] <- "W"
    if (U > threshold) res["U"] <- "U"
    if (B > threshold) res["B"] <- "B"
    if (R > threshold) res["R"] <- "R"
    if (G > threshold) res["G"] <- "G"

    paste(paste(res, collapse = ''))
}



mean_win_rate <-  mean(card_data$won)
mean_odds_win = mean_win_rate/(1-mean_win_rate)

all_data = list()

for (card in all_card_names) {
    print(card)
    
    my_label = paste0('GIH_', card)
    
    mytab <- table(card_data[[paste0('GIH_', card)]], card_data[['won']])
    fish = fisher.test(mytab[c(1,2),])
    
 
    unweighted_model <- glm(data = card_data, formula = paste0('won ~ `GIH_', card, '`'), family = binomial)
    weighted_model <- glm(data = card_data, formula = paste0('won ~ `GIH_', card, '`'), family = binomial, weights = 1/(7 + card_data$num_turns))
    odds_weighted <- mean_odds_win*exp(coef(weighted_model)[2])
    odds_unweighted <- mean_odds_win*exp(coef(unweighted_model)[2])

    GIH_win_rate_modelled_weighted <- odds_weighted/(1 + odds_weighted)
    GIH_win_rate_modelled_unweighted <- odds_unweighted/(1 + odds_unweighted)
    
    local_res = list(card = card,
                     OR = as.numeric(fish$estimate),
                     fisher_pvalue = as.numeric(fish$p.value),
                     GIH_WR_basic =  as.numeric(mytab["1",]['TRUE'] / sum(mytab["1",])),
                     GIH_win_rate_modelled_weighted = GIH_win_rate_modelled_weighted,
                     GIH_win_rate_modelled_unweighted = GIH_win_rate_modelled_unweighted,
                     W = cor(card_data[[ my_label]], card_data$deck_Plains),
                     U = cor(card_data[[ my_label]], card_data$deck_Island),
                     B = cor(card_data[[ my_label]], card_data$deck_Swamp),
                     R = cor(card_data[[ my_label]], card_data$deck_Mountain),
                     G = cor(card_data[[ my_label]], card_data$deck_Forest))
    
    local_res$color = f(local_res$B, local_res$G, local_res$R, local_res$U, local_res$W)

    relevant_decks <- dplyr::filter(card_data,  main_colors ==  local_res$color)
    mytab <- table(relevant_decks[[paste0('GIH_', card)]], relevant_decks[['won']])

    if (nrow(mytab) >= 2) nb_games_GIH_color_matched <- sum(mytab['1',]) else nb_games_GIH_color_matched <- 0
    local_res[['nb_games_GIH_color_matched']] <- nb_games_GIH_color_matched

    if (nb_games_GIH_color_matched > 20) { # we need at least 20 here to make sense of things
        fish_matched = fisher.test(mytab[c(1,2),])
        local_res[['OR_color_matched']] = as.numeric(fish_matched$estimate)
        local_res[['GIH_WR_color_matched']] =  as.numeric(mytab["1",]['TRUE'] / nb_games_GIH_color_matched)
    } else {
        local_res[['OR_color_matched']] = NA_real_
        local_res[['GIH_WR_color_matched']] = NA_real_
    }
    
    all_data[[ card ]] <- local_res
}

single_final_table = dplyr::bind_rows(all_data)
single_final_table =  single_final_table[ order(single_final_table$GIH_win_rate_modelled_weighted, decreasing = TRUE),]


single_final_table <- dplyr::filter(single_final_table,
                                   ! card %in% c("Mountain", "Plains", "Island", "Forest", "Swamp", "Verdant Catacombs", "Marsh Flats"))

write.csv(single_final_table, file = paste0('processed_data/single_card_analysis_', set, '.csv'))



interesting_cards <- dplyr::mutate(single_final_table, ratio = GIH_WR_color_matched/GIH_WR_basic) %>%
    dplyr::arrange(desc(ratio)) %>%
    dplyr::select(card, color, GIH_WR_basic, GIH_WR_color_matched, nb_games_GIH_color_matched)


print(interesting_cards)


