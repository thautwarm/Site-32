units, teens, tens:: [String]
units=["zero","one","two","three","four","five",
       "six","seven","eight","nine"]
teens=["ten","eleven","twelve","thirteen",
        "fourteen","fifteen","sixteen","seventten","nineteen"]
tens=["twenty","thirty","forty","fifty","sixty","seventy","eighty",
      "ninety"]

conv ::Int->String
conv n
    |n<0  = "negative "++conv (-n)
    |n<10 = units!!n
    |n<20 =let idx = mod n 10
            in teens!!idx
    |n<100 =
            let (idx1,idx2)=(div n 10,mod n 10)
            in if (mod n 10)/=0
    	            then
    	                tens!!(idx1-2) ++"-"++ units!!idx2
    	            else
    	        	    tens!!(idx1-2)
    |otherwise =
    	    let (divNumber,splitString) = 
    	    	    if n<1000 then (100," hundred") 
    	    	    else (1000," thousand") 
    	        (idx1,idx2) =(div n divNumber,mod n divNumber)
    	    in conv idx1 ++splitString++ 
    	        if idx2==0 then "" else " and " ++conv idx2



            


-- conv1 :: Int ->String
-- conv1 n = units!!n 


-- digits :: Int ->(Int,Int)
-- digits n =(div n 10 , mod n 10)

-- conv2 :: Int->String
-- conv2 = combine2.digits

-- combine2 ::(Int,Int)->String
-- combine2 (n2,n1) 
--     |n2==0 = units!!n1
--     |n2==1 = teens!!n1
--     |n2>=2 && n1==0 = tens!!(n2-2)
--     -- |n2>=2 && u/=0 = tens!!(n2-2) ++ units!!n1
--     |otherwise = tens!!(n2-2) ++"-"++ units!!n1



-- conv3 :: Int->String
-- conv3 n 
--     |m==0 = conv2 h
--     |h==1 = units!!m ++ " hundred "
--     |otherwise = units!!m ++" hundred an "++ conv2 h
--     where (m,h)=(div n 100,mod n 100)


