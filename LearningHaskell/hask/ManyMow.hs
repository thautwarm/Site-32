module ManyMow where
import Year17.Month5.Integer2NaturalLang
import Data.Char
typestr="*1 man went to mow\nWent to mow a meadow\n*2 and his dog\nWent to mow a meadow"
upperFirst ::String->String
upperFirst (x:y)=(toUpper x ): y
fix::Int->String
fix n=
    let fixp nn= upperFirst(conv nn) ++" man " ++  if nn>1 then ", "++fixp (nn-1) else "" 
        fixp::Int->String
        fixr::String->String
        fixr ('*':'1':y)=upperFirst (conv n) ++ fixr y
    	fixr ('*':'2':y)=fixp n ++ y
        fixr (x:y)=x:(fixr y)
    in fixr typestr






