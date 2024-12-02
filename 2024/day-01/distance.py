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
    df = pd.DataFrame({
        "list_1": list(in_df.list_1.sort_values()),
        "list_2": list(in_df.list_2.sort_values()),
    })
    df["distance"] = abs(df["list_1"] - df["list_2"])
    total_distance = df["distance"].sum()
    print(total_distance)

if __name__ == "__main__":
    main()
    
    
        
