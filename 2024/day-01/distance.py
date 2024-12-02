# https://adventofcode.com/2024/day/1

import pandas as pd
import sys

def main():
    in_file = sys.argv[1]

    # Use pandas to parse the two columns of
    # numbers separated by spaces.
    in_df = pd.read_table(
        in_file,
        names=["list_1", "list_2"],
        sep="  ",
        engine="python",
    )

    # Create a new dataframe with the columns
    # in sorted order.
    df = pd.DataFrame({
        "list_1": list(in_df.list_1.sort_values()),
        "list_2": list(in_df.list_2.sort_values()),
    })

    # Create `distance` column with row-wise abs differences
    df["distance"] = abs(df["list_1"] - df["list_2"])

    # Output value is sum of distances
    total_distance = df["distance"].sum()
    print(total_distance)

if __name__ == "__main__":
    main()
    
    
        
