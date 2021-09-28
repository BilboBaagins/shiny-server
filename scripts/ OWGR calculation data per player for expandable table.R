# OWGR calculation data per player for expandable table. 




# Maybe left join to the "Previous 8 majors table", so missing majors show up as NAs

data[data$Player == "Billy Archbold", ]

test <- left_join(
    last_eight_majors_df,
    data[data$Player == "Billy Archbold", c
        ("Major",
        "Player", 
        "Handicap", 
        "Score", 
        "playoff_win",
        "Pos",
        "Weighted_Score",
        "Pts",
        "Weighted_Pts"
        )
    ], 
    by = "Major"
) 



