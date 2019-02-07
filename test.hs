phrase x = if x==True then "Yeah." else "I don't think so..."

idSpaces l = [x | x<- [0..(length l -1)], l!!x==' ']
wordify l = [[ l!!x | x <- [(([0]++idSpaces l )!!a)..((idSpaces l ++[(length l - 1)])!!a)], l!!x/=' ', not(elem (l!!x) (['['..'`']++[':'..'@']++".,?{}+-~!"))] | a<- [0..length (idSpaces l)] ]

check w = if (lowercase w=="not" || lowercase w=="no") then "False" else 
        if (elem (lowercase w) ["school", "schools", "homework", "HW", "class", "classwork", "schoolwork"]) then "School" else
        if (elem (lowercase w) ["food", "breakfast", "snack", "lunch", "dinner", "meal"]) then "Eating" else
        "none"
flavor l = if (elem "School" [check x | x<-(wordify l)] ) then "Try to have fun at class, maybe." else
        if (elem "Eating" [check x | x<-(wordify l)] ) then "Don't feel too hungry. Food awaits." else
        "I dunno."

lowify l = [ ['a'..'z']!!n | n<- [0..25], l==['A'..'Z']!!n]
lowercase x = [ if x `elem` ['A'..'Z'] then ((lowify x)!!0) else x | x<- x]