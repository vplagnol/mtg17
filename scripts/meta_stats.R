set <- 'TLA'

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



if (set == 'TDM') {
    list_removals_W <- c('Static Snare', 'Coordinated Maneuver', 'Osseous Exhale', 'Rally the Monastery', 'Stormplain Detainment', 'Clarion Conqueror')
    list_removals_U <- c('Ringing Strike Mastery', "Ureni's Rebuff", 'Constrictor Sage', 'Riverwalk Technique', 'Iceridge Serpent', 'Marang River Regent')
    list_removals_B <- c('Worthy Cost', 'Caustic Exhale', 'Desperate Measures', 'Strategic Betrayal', "Dragon's Prey", 'Wail of War', 'Salt Road Skirmish', 'Gurmag Rakshasa')
    list_removals_R <- c('Channeled Dragonfire', 'Stadium Headliner', 'Molten Exhale', 'Twin Bolt', 'Sunset Strikemaster', 'Overwhelming Surge', "Narset's Rebuke")
    list_removals_G <- c('Piercing Exhale', "Sarkhan's Resolve", 'Heritage Reclamation', 'Undergrowth Leopard', 'Disruptive Stormbrood', 'Knockout Maneuver', 'Rite of Renewal')

    all_removals <- unique(c(list_removals_W, list_removals_U, list_removals_B, list_removals_R, list_removals_G))
}

if (set == 'DFT') {
    list_removals_W <- c('Collision Course', "Ride's End", 'Gallant Strike', 'Perilous Snare', 'Spectacular Pileup', 'Detention Chariot')
    list_removals_U <- c('Flood the Engine', 'Sabotage Strategist', 'Trade the Helm', 'Possession Engine', 'Bounce Off', 'Roadside Blowout', 'Stall Out', 'Trip Up', 'Spikeshell Harrier')
    list_removals_B <- c('Hellish Sideswipe', 'Locust Spray', 'Demonic Junker', 'Momentum Breaker', 'Quag Feast', 'Spin Out', 'Syphon Fuel', 'Shefet Archfiend', 'Intimidation Tactics', 'Gastal Raider', 'Ripclaw Wrangler', 'Ancient Vendetta')
    list_removals_R <- c('Dynamite Diver', 'Road Rage', 'Lightning Strike', 'Fuel the Flames', 'Outpace Oblivion', 'Crash and Burn', 'Boommobile', 'Dracosaur Auxiliary', 'Skycrash', 'Gastal Blockbuster', 'Broadside Barrage')
    list_removals_G <- c('Run Over', 'Plow Through', 'Broken Wings', 'District Mascot', 'Webstrike Elite')

    all_removals <- unique(c(list_removals_W, list_removals_U, list_removals_B, list_removals_R, list_removals_G))
}

if (set == 'ECL') {
    list_removals_W <- c('Crib Swap', 'Keep Out', 'Protective Response')
    list_removals_U <- c('Liminal Hold', 'Swat Away', 'Temporal Cleansing', 'Wanderwine Farewell')
    list_removals_B <- c("Auntie's Sentence", 'Blight Rot', "Bogslither's Embrace", 'Nameless Inversion', 'Requiting Hex')
    list_removals_R <- c('Boulder Dash', 'Cinder Strike', 'Explosive Prodigy', 'Feed the Flames', 'Sear', 'Tweeze')
    list_removals_G <- c('Assert Perfection', 'Chomping Changeling', 'Pitiless Fists', 'Unforgiving Aim', 'Vinebred Brawler')

    all_removals <- unique(c(list_removals_W, list_removals_U, list_removals_B, list_removals_R, list_removals_G))
}

if (set == 'TLA') {
    list_removals_W <- c("Aang's Iceberg", "Airbender's Reversal", "Avatar's Wrath", 'Earth Kingdom Jailer', 'Path to Redemption', "Sandbenders' Storm")
    list_removals_U <- c('Watery Grasp')
    list_removals_B <- c('Day of Black Sun', 'Deadly Precision', 'Epic Downfall', 'Heartless Act', 'Sold Out', 'The Rise of Sozin')
    list_removals_R <- c('Combustion Technique', 'Bumi Bash', 'The Last Agni Kai')
    list_removals_G <- c('Earth Rumble', 'Rocky Rebuke')

    all_removals <- unique(c(list_removals_W, list_removals_U, list_removals_B, list_removals_R, list_removals_G))
}

card_data[['total_removals']] <- 0
for (card in all_removals) {
    message(card, ': ', sum(card_data[, paste0('GIH_', card)]))
    card_data[['total_removals']] <- card_data[['total_removals']] + card_data[, paste0('GIH_', card)]
}


library(dplyr)
library(ggplot2)

wr_per_removal <- dplyr::group_by(card_data, total_removals) %>% dplyr::summarise(n = n(),
                                                                                  win_rate = sum(won)/n())

## Proportion of removal cards among all cards seen (hand + drawn)
gih_cols <- grep("^GIH_", names(card_data), value = TRUE)
card_data[['total_gih']] <- rowSums(card_data[, gih_cols])
card_data[['prop_removals']] <- as.numeric(unlist(card_data[['total_removals']])) / card_data[['total_gih']]

## Bin into 5% intervals and summarise win rate
bin_width <- 0.05
card_data[['prop_bin']] <- round(card_data[['prop_removals']] / bin_width) * bin_width

wr_per_prop <- card_data %>%
    group_by(prop_bin) %>%
    summarise(
        n        = n(),
        win_rate = sum(won) / n(),
        se       = sqrt(win_rate * (1 - win_rate) / n())
    ) %>%
    filter(n >= 50)

p <- ggplot(wr_per_prop, aes(x = prop_bin, y = win_rate)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = win_rate - 1.96 * se, ymax = win_rate + 1.96 * se), width = 0.005) +
    geom_point(aes(size = n)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        x     = "Proportion of removal cards among cards seen (hand + drawn)",
        y     = "Win rate",
        title = paste("Win rate vs removal proportion -", set),
        size  = "# games"
    ) +
    theme_bw()

ggsave(filename = paste0("plots/winrate_removals_", set, ".png"), plot = p, width = 8, height = 5, dpi = 150)
