from bs4 import BeautifulSoup as bs4, Tag
from urllib.parse import urlparse, ParseResult
from wisepy2 import wise
from subprocess import check_call
from collections import defaultdict
from pathlib import Path
import asyncio
import os.path as op
import re

"""
<script src="./Powerful Pattern Matching in Flowpython - 知乎_files/vendor.6c16e03dca561b828324.js">
</script>
"""

url = re.compile("(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?")

netlocs_to_local = {
    'pic1.zhimg.com': "pic1_zhimg",
    'pic2.zhimg.com': 'pic2_zhimg',
    'pic3.zhimg.com': 'pic3_zhimg',
    'pic4.zhimg.com': 'pic4_zhimg',
}

to_download = []
cur_path = None

def download_as(src, dest):
    to_download.append((src, dest))

async def perform_download(src, dest):
    dir = op.split(dest)[0]
    Path(dir).mkdir(exist_ok=True, parents=True)
    check_call(["wget", src, '-O', dest])
    await asyncio.sleep(0.1)

def proceed_url(url):
    o = urlparse(url)
    subs = netlocs_to_local.get(o.netloc)
    if subs:
        localpath = subs + o.path
        download_as(url, op.join(cur_path, localpath))
        return True, op.join('.', localpath)
    else:
        return False, url

def remove_zhimage(attrs):
    if 'data-original' in attrs:
        new = {
            'src': attrs['data-original'],
            'height': attrs['data-rawheight'],
            'width': attrs['data-rawwidth'],
            "class": "center"
        }
        attrs.clear()
        attrs.update(new)

def proceed(node):
    def proceed_attr_value(value):
        urls = url.findall(value)
        flag = False
        for each in urls:
            each = '{0}://{1}{2}'.format(*each)
            t, new = proceed_url(each)
            if not t:
                continue
            flag = True
            value = value.replace(each, new)
        return flag, value

    if not isinstance(node, (bs4, Tag)):
        return

    attrs = node.attrs
    for key in list(attrs.keys()):
        value = attrs[key]
        if isinstance(value, str):
            flag, value = proceed_attr_value(value)
        elif isinstance(value, list):
            flags, value = zip(*map(proceed_attr_value, value))
            flag = any(flags)
        else:
            print(value)
            raise TypeError(type(value))
        if flag:
            attrs[key] = value

    if node.name == 'img' and remove_zhimage(node.attrs):
        pass

    for each in node:
        proceed(each)

async def run_all(elts):
    res = await asyncio.gather(*elts)
    return res

@wise
def read(filename: str):
    global cur_path
    cur_path = op.abspath(op.split(filename)[0])
    with open(filename) as f:
        s = f.read()
    node = bs4(s)
    proceed(node)
    asyncio.run(run_all(perform_download(*each) for each in to_download))

    with open(filename, 'w') as f:
        f.write(str(node))
if __name__ == '__main__':
    import sys
    read(sys.argv[1:])