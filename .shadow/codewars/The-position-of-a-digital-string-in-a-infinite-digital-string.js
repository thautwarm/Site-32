
function chunkleft(arr, n){
	if (arr.startsWith('0'))
		return NaN
	var begin = parseInt(arr.slice(0,n))
	var now   = begin
	var end   = n
	var end_r = NaN ,test = NaN
	while ( end != arr.length ){
		++now;
		end_r = end+String(now).length
		test  =  parseInt(arr.slice(end, end_r))
		if (test!=now){
			if (end_r>arr.length && String(now).startsWith(arr.slice(end)))
				return [begin, 0]
			return NaN
		}
		end = end_r
	}
	return [begin , 0]
}
function chunkright(arr, n){
	if  (arr.slice(-n).startsWith('0'))
		 return NaN
	var now   = parseInt(arr.slice(-n));
	var begin = arr.length - n;
	var begin_r = NaN, test = NaN;
	while (begin !=0 ){
		-- now;
		begin_r =  begin - String(now).length;
		test    =  parseInt(arr.slice(Math.max(0, begin_r), begin)) 
		if (test != now){
			if (begin_r<0 && String(now).endsWith(arr.slice(0, begin)))
				return [now, -begin_r]
			return NaN
		}
		begin = begin_r
	} 
	return [now, 0]
}
function chunk(arr, n){
	return chunkleft(arr, n) || chunkright(arr, n)
}
let level_dict = class{}
level_dict.level = {0:0}
level_dict.sumal = {0:0}
level_dict.maxmal = 0;
level_dict.index = function(v){
	v-=1
	if (v<0) return 0
	var length = String(v).length
	if ( !( (length-1) in level_dict.level) ){
		[...Array(length).keys()].slice(level_dict.maxmal+1).forEach( 
				i =>{
					
					level_dict.level[i] = i*9*(Math.pow(10,i-1))
					level_dict.sumal[i] = level_dict.sumal[i-1] + level_dict.level[i]

			}
		)
		level_dict.maxmal = length-1
	}
	var begin = parseInt("1"+"0".repeat(length-1))
	return level_dict.sumal[length-1]+(v - begin+1)*length
}
function add1(string){
	 return String(parseInt("1"+string)+1).slice(1)
}
	
function get_hidden(string, n){
	var ret  = []
	var tail = string.slice(-n)
	    return [...Array(n).keys()].map( 
		i=>{
			return string+add1(tail.slice(0,i))
		}
	)
}
function naive_find_position(string){
	var tup = NaN;
	var breaker = false;
	[...Array(string.length).keys()].forEach(
		i=>{
			if (!breaker){
				tup = chunk(string ,i+1);
				if (tup)
					breaker = true;
			}
		}
	)
	if (!tup) return -1
	var a = tup[0], b = tup[1]
	return level_dict.index(a)+b	
}

function all(seq){
	var i ;
	for(i = 0; i<seq.length; ++i)
		if (! seq[i] ) return false
	return true	
}

function findPosition(string){
	var arr= string.split("")
	if(all(arr.map( i=> i == "9")))
		return findPosition(string+'0')
	else if (all(arr.map( i=> i == "0")))
		return level_dict.index(parseInt('1'+string))+1
	else{
		return Math.min.apply(Math, [...Array(string.length).keys()].map(
				i =>{
				  return  get_hidden(string, i+1).map(naive_find_position).filter(x=>x!=-1)
				}
		).reduce((x,y) => x.concat(y)))
	}
}