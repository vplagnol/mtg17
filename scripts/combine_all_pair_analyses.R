library(dplyr)

results <- list()
all_files <- list.files(path = "temp/", pattern = "pairs_chunk_*")
for (file in all_files) results[[ file ]] <- readr::read_csv(paste0("temp/", file))

combined <- dplyr::bind_rows(results)

basic_lands <- c("Forest", "Swamp", "Plains", "Mountain", "Island")

clean_results <- dplyr::filter(combined, count > 400, interaction_pval < 1e-4) %>% dplyr::arrange(desc(interaction_term))
clean_results <- dplyr::filter(clean_results, ! card1 %in% basic_lands, ! card2 %in% basic_lands)

print(tail(as.data.frame(clean_results), 10))

print(head(as.data.frame(clean_results), 10))
