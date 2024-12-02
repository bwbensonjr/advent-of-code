# https://adventofcode.com/2024/day/1

from collections import Counter
import pandas as pd
import sys

def main():
    in_file = sys.argv[1]

    # Use pandas to read number columns into
    # a DataFrame.
    in_df = pd.read_table(
        in_file,
        names=["list_1", "list_2"],
        sep="  ",
        engine="python",
    )

    # `collections.Counter` is great for counting
    # occurences; used for second column.
    list_2_counts = Counter(in_df.list_2)
    
    # Iterate over first column multiplying the
    # number by its count in second column.
    similarity = 0
    for list_1_val in in_df.list_1:
        similarity += list_1_val * list_2_counts[list_1_val]
        
    print(similarity)

if __name__ == "__main__":
    main()
    
    
        
