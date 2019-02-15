phrase x = if x==True then "Yeah." else "I don't think so..."


idSpaces :: [Char] -> [Int]
wordify :: String -> [[Char]]
check :: [Char] -> [Char]
lowify :: Char -> [Char]
lowercase :: [Char] -> [Char]

idSpaces l = [x | x<- [0..(length l -1)], l!!x==' ']
wordify l = [[ l!!x | x <- [(([0]++idSpaces l )!!a)..((idSpaces l ++[(length l - 1)])!!a)], l!!x/=' ', not(elem (l!!x) (['['..'`']++[':'..'@']++".,?{}+-~!"))] | a<- [0..length (idSpaces l)] ]

check w 
        | (lowercase w=="not" || lowercase w=="no") = "Negation"
        | find ["school", "schools", "homework", "hw", "class", "classwork", "schoolwork"] = "TOP_School"
        | find ["food", "breakfast", "snack", "lunch", "dinner", "meal"] = "TOP_Eating"
        | find ["hungry", "starving", "stomach"] = "TOP_Hunger"
        | find ["math", "mathematics", "calc", "calculus", "algebra", "alg", "decimal", "decimals", "fractions", "fraction", "multiplication", "exponent", "exponents", "division"] = "TOP_Math"
        | find ["joy", "joyous", "great", "awesome", "finally", "excited", "thrilled", "thrill"] = "TNM_Celebrate"
        | find ["tired", "sleepy", "fatigued", "exhausted"] = "TNM_Tired"
        | otherwise = "none"
        where find a = elem (lowercase w) a
flavor :: [Char] -> [Char]
flavor l = let find a = elem a [check x | x<-(wordify l)] in 
        (if find "TNM_Celebrate" then "Yeah! " else 
        if find "TNM_Tired" then "You're really working. " else 
        "") ++
        --These are the topic-specific statements which are probably the meat, for this function.
        (if (find "TOP_Math" && find "TOP_School") then "It's awesome that you can take math courses" else
        if (find "TOP_Hunger" && find "TOP_Eating") then "Have something. I'll keep you company" else
        if (find "TOP_School" && find "TOP_Eating") then "I wonder if the cafeteria food is any good" else
        if find "TOP_School" then "Try to have fun at class, maybe" else
        if find "TOP_Hunger" then "Since you need organic fuel? That sounds cool" else
        if find "TOP_Math" then "I love math" else
        if find "TOP_Eating" then "Don't feel too hungry. Food awaits" else
        "I dunno") ++
        --This is additional comment stuff to do with miscellaneous
        (if find "Negation" then ", but I don't know. I'm probably wrong." else 
        if last l == '!' then "!" else ".")

lowify l = [ ['a'..'z']!!n | n<- [0..25], l==['A'..'Z']!!n]
lowercase x = [ if x `elem` ['A'..'Z'] then ((lowify x)!!0) else x | x<- x]
