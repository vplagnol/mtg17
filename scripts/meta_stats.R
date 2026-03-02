library(dplyr)
library(ggplot2)

sets       <- c('DFT', 'TLA', 'ECL')
color_pairs <- c('WU', 'WB', 'WR', 'WG', 'UB', 'UR', 'UG', 'BR', 'BG', 'RG')
bin_width  <- 0.05

## Removal lists by set and color
removals <- list(
    TDM = unique(c(
        'Static Snare', 'Coordinated Maneuver', 'Osseous Exhale', 'Rally the Monastery', 'Stormplain Detainment', 'Clarion Conqueror',  ## W
        'Ringing Strike Mastery', "Ureni's Rebuff", 'Constrictor Sage', 'Riverwalk Technique', 'Iceridge Serpent', 'Marang River Regent',  ## U
        'Worthy Cost', 'Caustic Exhale', 'Desperate Measures', 'Strategic Betrayal', "Dragon's Prey", 'Wail of War', 'Salt Road Skirmish', 'Gurmag Rakshasa',  ## B
        'Channeled Dragonfire', 'Stadium Headliner', 'Molten Exhale', 'Twin Bolt', 'Sunset Strikemaster', 'Overwhelming Surge', "Narset's Rebuke",  ## R
        'Piercing Exhale', "Sarkhan's Resolve", 'Heritage Reclamation', 'Undergrowth Leopard', 'Disruptive Stormbrood', 'Knockout Maneuver', 'Rite of Renewal'  ## G
    )),
    DFT = unique(c(
        'Collision Course', "Ride's End", 'Gallant Strike', 'Perilous Snare', 'Spectacular Pileup', 'Detention Chariot',  ## W
        'Flood the Engine', 'Sabotage Strategist', 'Trade the Helm', 'Possession Engine', 'Bounce Off', 'Roadside Blowout', 'Stall Out', 'Trip Up', 'Spikeshell Harrier',  ## U
        'Hellish Sideswipe', 'Locust Spray', 'Demonic Junker', 'Momentum Breaker', 'Quag Feast', 'Spin Out', 'Syphon Fuel', 'Shefet Archfiend', 'Intimidation Tactics', 'Gastal Raider', 'Ripclaw Wrangler', 'Ancient Vendetta',  ## B
        'Dynamite Diver', 'Road Rage', 'Lightning Strike', 'Fuel the Flames', 'Outpace Oblivion', 'Crash and Burn', 'Boommobile', 'Dracosaur Auxiliary', 'Skycrash', 'Gastal Blockbuster', 'Broadside Barrage',  ## R
        'Run Over', 'Plow Through', 'Broken Wings', 'District Mascot', 'Webstrike Elite'  ## G
    )),
    ECL = unique(c(
        'Crib Swap', 'Keep Out', 'Protective Response',  ## W
        'Liminal Hold', 'Swat Away', 'Temporal Cleansing', 'Wanderwine Farewell',  ## U
        "Auntie's Sentence", 'Blight Rot', "Bogslither's Embrace", 'Nameless Inversion', 'Requiting Hex',  ## B
        'Boulder Dash', 'Cinder Strike', 'Explosive Prodigy', 'Feed the Flames', 'Sear', 'Tweeze',  ## R
        'Assert Perfection', 'Chomping Changeling', 'Pitiless Fists', 'Unforgiving Aim', 'Vinebred Brawler'  ## G
    )),
    TLA = unique(c(
        "Aang's Iceberg", "Airbender's Reversal", "Avatar's Wrath", 'Earth Kingdom Jailer', 'Path to Redemption', "Sandbenders' Storm",  ## W
        'Watery Grasp',  ## U
        'Day of Black Sun', 'Deadly Precision', 'Epic Downfall', 'Heartless Act', 'Sold Out', 'The Rise of Sozin',  ## B
        'Combustion Technique', 'Bumi Bash', 'The Last Agni Kai',  ## R
        'Earth Rumble', 'Rocky Rebuke'  ## G
    ))
)

## Loop over sets and color pairs, accumulate results
all_results <- list()

for (set in sets) {
    message("Processing set: ", set)
    gih_file <- paste0("processed_data/game_data_", set, "_reduced_GIH.csv")

    if (file.exists(gih_file)) {
        message("  Loading cached GIH file")
        card_data <- readr::read_csv(gih_file, show_col_types = FALSE)
    } else {
        message("  Computing GIH columns from raw file")
        iFile <- paste0("processed_data/game_data_", set, "_reduced.csv")
        card_data <- readr::read_csv(iFile, show_col_types = FALSE)

        ## Compute GIH (games in hand) columns
        all_card_names <- gsub(grep(names(card_data), pattern = "drawn_", value = TRUE),
                               pattern = "drawn_", replacement = "")
        for (card in all_card_names) {
            card_data[[ paste0('GIH_', card) ]] <- card_data[[ paste0('drawn_', card) ]] +
                                                   card_data[[ paste0('opening_hand_', card) ]]
        }
        card_data <- card_data[ , -grep(pattern = "^opening", x = names(card_data)) ]
        card_data <- card_data[ , -grep(pattern = "^drawn",   x = names(card_data)) ]

        readr::write_csv(card_data, file = gih_file)
        message("  Saved GIH file to ", gih_file)
    }

    ## Compute total removals
    card_data[['total_removals']] <- 0
    for (card in removals[[set]]) {
        card_data[['total_removals']] <- card_data[['total_removals']] + card_data[[ paste0('GIH_', card) ]]
    }

    ## Proportion of removals among all cards seen
    gih_cols <- grep("^GIH_", names(card_data), value = TRUE)
    card_data[['total_gih']]    <- rowSums(card_data[, gih_cols])
    card_data[['prop_removals']] <- as.numeric(unlist(card_data[['total_removals']])) / card_data[['total_gih']]
    card_data[['prop_bin']]     <- round(card_data[['prop_removals']] / bin_width) * bin_width

    ## Summarise per color pair
    for (cp in color_pairs) {
        subset_data <- card_data[ card_data[['main_colors']] == cp, ]
        if (nrow(subset_data) == 0) next

        wr_cp <- subset_data %>%
            group_by(prop_bin) %>%
            summarise(
                n        = n(),
                win_rate = sum(won) / n(),
                se       = sqrt(win_rate * (1 - win_rate) / n()),
                .groups  = 'drop'
            ) %>%
            filter(n >= 30) %>%
            mutate(set = set, color_pair = cp)

        all_results[[ paste(set, cp, sep = "_") ]] <- wr_cp
    }
}

plot_data <- bind_rows(all_results)
plot_data[['set']]        <- factor(plot_data[['set']],        levels = sets)
plot_data[['color_pair']] <- factor(plot_data[['color_pair']], levels = color_pairs)

p <- ggplot(plot_data, aes(x = prop_bin, y = win_rate)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = win_rate - 1.96 * se, ymax = win_rate + 1.96 * se), width = 0.005) +
    geom_point(aes(size = n)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0.35, 0.65)) +
    facet_grid(color_pair ~ set) +
    labs(
        x    = "Proportion of removal cards among cards seen (hand + drawn)",
        y    = "Win rate",
        size = "# games"
    ) +
    theme_bw(base_size = 14) +
    theme(
        strip.text = element_text(size = 9),
        axis.text  = element_text(size = 8)
    )

print(p)
ggsave(filename = "plots/winrate_removals_by_archetype.png", plot = p,
       width = 14, height = 20, dpi = 150)
