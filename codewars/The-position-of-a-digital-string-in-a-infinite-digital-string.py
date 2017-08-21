map    = lambda f,*seqs: (f(*items) for items in zip(*seqs) )   
  
def takewhen(arr, f ,rev= False):
    if rev:
        for i,it in enumerate(arr[::-1]):
            if (f(it)): 
                return len(arr)-i-1
    else:
        for i,it in enumerate(arr):
            if (f(it)): 
                return i
    return -1

def chunkleft(arr, n):
    if arr.startswith('0'): return None
    begin = int(arr[:n])
    now   = begin
    end   = n
    while end != len(arr):
        now += 1
        end_r = end+len(str(now))
        test  = int(arr[end:end_r])
        if test!=now:
            if end_r > len(arr) and str(now).startswith(arr[end:]):
                return ( begin, 0 )
            return None
        end =  end_r
    return (begin,0)

def chunkright(arr, n):
    if   arr[-n:].startswith('0'): return None    
    now     = int( arr[-n:] )
    begin   = len(arr) - n
    while begin != 0:
        now   -= 1
        begin_r = begin-len(str(now))
        test  = int(arr[max(0, begin_r):begin] )
        if test!=now:
            if begin_r < 0 and  str(now).endswith(arr[:begin]):
                return (now,  0-begin_r )
            return None
        begin = begin_r
    return (now, 0)

def chunk(arr, n):
    return chunkleft(arr, n) or chunkright(arr, n)

class level_dict:
    level = dict({0:0})
    sumal = dict({0:0})
    maxmal = 0
    def __new__(self):
        return self
    def index(v):
        v-=1
        level = level_dict.level
        sumal = level_dict.sumal
        if v < 0 : return 0
        length = len(str(v))
        if length-1 not in level:
            for i in range(level_dict.maxmal+1, length):
                level[i] = i*9*(10**(i-1))
                sumal[i] = sumal[i-1]+level[i]
            level_dict.maxmal = length-1
        begin = int("1"+"0"*(length-1))
        return sumal[length-1]+(v - begin+1)*length
            
def add1(string):
    return str(int("1"+string)+1)[1:]
    
def get_hidden(string, n):
    ret  = []
    tail = string[-n:] 
    for i in range(0,n):
        ret.append( string + add1(tail[:i]))
    return ret
    
    

def naive_find_position(string):
    i = 1
    tup = None
    for i in range(1, len(string)+1):
        tup = chunk(string, i)
        if tup:
            break
        i+=1
    if not tup: return -1
    a,b = tup
    return level_dict().index(a)+b

def find_position(string):
        if all(map(lambda x:x == '9', string)):
            return find_position(string+'0')
        elif all(map(lambda x:x=='0', string)):
            return level_dict.index(int('1'+string))+1
        else:
            res = []
            for i in range(1, len(string)+1):
                chunks = get_hidden(string, i)
                res.append(tuple(filter(lambda x:x!=-1, map(naive_find_position, chunks))))
            return min(sum(res, ()))