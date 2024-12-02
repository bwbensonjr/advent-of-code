from collections import Counter
import pandas as pd
import sys

def main():
    in_file = sys.argv[1]
    in_df = pd.read_table(
        in_file,
        names=["list_1", "list_2"],
        sep="  ",
        engine="python",
    )
    similarity = 0
    list_2_counts = Counter(in_df.list_2)
    for list_1_val in in_df.list_1:
        similarity += list_1_val * list_2_counts[list_1_val]
    print(similarity)

if __name__ == "__main__":
    main()
    
    
        
