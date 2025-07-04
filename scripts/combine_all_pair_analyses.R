library(dplyr)
##set <- 'Tarkir'
##set <- 'Aetherdrift'
set <- 'FinalFantasy'

results <- list()
all_files <- list.files(path = paste0("temp/", set), pattern = "pairs_chunk_*")
for (file in all_files) results[[ file ]] <- readr::read_csv(paste0("temp/", set, "/", file))

combined <- dplyr::bind_rows(results)

basic_lands <- c("Forest", "Swamp", "Plains", "Mountain", "Island")
nonbasic_lands <- c("Bloodfell Caves", "Blossoming Sands", "Swiftwater Cliffs", "Scoured Barrens", "Tranquil Cove", "Dismal Backwater")
exclude_lands <- c(basic_lands, nonbasic_lands)

clean_results <- dplyr::filter(combined, count > 400, interaction_pval < 1e-4) %>% dplyr::arrange(desc(interaction_term))
clean_results <- dplyr::filter(clean_results, ! card1 %in% exclude_lands, ! card2 %in% exclude_lands)

print(tail(as.data.frame(clean_results), 30)[, -1])

print(head(as.data.frame(clean_results), 40)[, -1])


print(all_files)

write.csv(clean_results, file = paste0("processed_data/", set, "_pair_analysis.csv"))
