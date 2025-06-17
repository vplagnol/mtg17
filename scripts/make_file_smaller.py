## remove information that is not related to "drawn"

import csv

iFile="data/game_data_public.TDM.PremierDraft.csv"; oFile="processed_data/game_data_Tarkir_reduced.csv"
iFile="data/game_data_public.DFT.PremierDraft.csv"; oFile="processed_data/game_data_Aetherdrift_reduced.csv"


target_strings = {"deck", "sideboard", "tutored"}

file=open(oFile, 'w', newline='', encoding='utf-8')
writer = csv.writer(file)

# Read CSV file
with open(iFile) as fp:
    csv_reader = csv.reader(fp, delimiter=",", quotechar='"')
    # Read the header row first (optional)
    try:
        header = next(csv_reader)
        non_matched_index = [
            index
            for index, item in enumerate(header) if all(target_str not in item for target_str in target_strings) or item in [ "deck_Island", "deck_Swamp", "deck_Plains", "deck_Mountain", "deck_Forest"]
        ]
        writer.writerow([header[index] for index in non_matched_index])
    except StopIteration:
        print("CSV file is empty.")
        
    for row in csv_reader:
        # Each 'row' is a list of strings
        writer.writerow( [ row[index] for index in non_matched_index])
