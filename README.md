
# Utility files to reproduce key analyses (as well as additional ones) from the 17Lands site


### Step 1: Download 17 lands data

The scripts expect the data to be found at this location (in the case of OTJ, i.e. Outlaws of Thunder Junction).
```data/game_data_public.OTJ.PremierDraft.csv```

### Step 2: Reduce the file input for R processing

The file ```scripts/make_file_smaller.py``` will take the input above and, if you edit the top section, create a smaller version in
```processed_data/game_data_OTJ_reduced.csv```



## Generate the single card analysis

This is done using the following R script:
```Rscript  scripts/single_card_analysis.R```


## Generate paired cards analysis

The analysis of pairs of cards (i.e. find the cards that combo particularly well) can be found in:
```Rscript scripts/pair_analysis.R```
