
作个无聊的记录
==========

说好了我不喜欢mssql。


.. code:: lsql



    <<Functions>>
    	round(var : number,N:int) 									-> #  retain
    	<> 															-> /=
    	between	: (front=>var) (a:comparable and b:comparable) 		-> filter (\x-> (x-a)*(x-b) >0 ) var
    	like 														-> # fuzzy matching
    	in/not in         											-> # like "in" in Python.
    	is (not) null												-> # filter (\x : isnull(x))
    <<Pattern-Matching>>
    	%															-> *
    	_															-> [\w|\W]{1}


    <<Operations>>
    	Create:
    		CREATE
    			DATABASE <dbName>
    				on primary(
    						name = <logical - name>
    						filename = <driver - location>
    						size =
    						maxSize =
    						fileGrowth =
    						)
    					log on(
    						name = <logical - name>
    						filename = <driver - location>
    						size =
    						maxSize =
    						fileGrowth =
    						)
    			TABLE <tableName>(
    				[attrs  valueType <optional: primary>
    							  <optional: not not>
    							  <optional: primary key>
    							  <optional: unique>
    							  <optional: default default-value>
    							  <optional: references otherTable(someAttr_in_the_table)>
    							  <optional: as {expr with other attrs in this table}>
    				,
    				...]
    							)

    <<Data-Structure>>
    	"%4Year-%2Month-%2Day" <=> "%Day/%Month/%Year"
    	""
