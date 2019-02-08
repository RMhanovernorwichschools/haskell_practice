phrase x = if x==True then "Yeah." else "I don't think so..."

idSpaces l = [x | x<- [0..(length l -1)], l!!x==' ']
wordify l = [[ l!!x | x <- [(([0]++idSpaces l )!!a)..((idSpaces l ++[(length l - 1)])!!a)], l!!x/=' ', not(elem (l!!x) (['['..'`']++[':'..'@']++".,?{}+-~!"))] | a<- [0..length (idSpaces l)] ]

check w = if (lowercase w=="not" || lowercase w=="no") then "Negation" else 
        if (elem (lowercase w) ["school", "schools", "homework", "hw", "class", "classwork", "schoolwork"]) then "TOP_School" else
        if (elem (lowercase w) ["food", "breakfast", "snack", "lunch", "dinner", "meal"]) then "TOP_Eating" else
        if (elem (lowercase w) ["hungry", "starving", "stomach"]) then "TOP_Hunger" else
        if (elem (lowercase w) ["joy", "joyous", "great", "awesome", "finally"]) then "TNM_Celebrate" else
        if (elem (lowercase w) ["tired", "sleepy", "fatigued", "exhausted"]) then "TNM_Tired" else
        "none"
flavor l = (if elem "TNM_Celebrate" [check x | x<-(wordify l)] then "Yeah! " else 
        if elem "TNM_Tired" [check x | x<-(wordify l)] then "You're really working. " else 
        "") ++
        --These are the topic-specific statements which are probably the meat, for this function.
        (if (elem "TOP_Hunger" [check x | x<-(wordify l)]) && (elem "TOP_Eating" [check x | x<-(wordify l)]) then "Have something. I'll keep you company" else
        if (elem "TOP_School" [check x | x<-(wordify l)]) && (elem "TOP_Eating" [check x | x<-(wordify l)]) then "I wonder if the cafeteria food is any good" else
        if (elem "TOP_School" [check x | x<-(wordify l)] ) then "Try to have fun at class, maybe" else
        if (elem "TOP_Hunger" [check x | x<-(wordify l)] ) then "Since you need organic fuel? That sounds cool" else
        if (elem "TOP_Eating" [check x | x<-(wordify l)] ) then "Don't feel too hungry. Food awaits" else
        "I dunno") ++
        --This is additional comment stuff to do with miscellaneous
        (if elem "Negation" [check x | x<-(wordify l)] then ", but I don't know. I'm probably wrong." else ".")

lowify l = [ ['a'..'z']!!n | n<- [0..25], l==['A'..'Z']!!n]
lowercase x = [ if x `elem` ['A'..'Z'] then ((lowify x)!!0) else x | x<- x]
