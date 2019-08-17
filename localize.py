from bs4 import BeautifulSoup as bs4, Tag
from urllib.parse import urlparse, ParseResult
from wisepy2 import wise
from subprocess import check_call
from collections import defaultdict
import os.path as op
import re

"""
<script src="./Powerful Pattern Matching in Flowpython - 知乎_files/vendor.6c16e03dca561b828324.js">
</script>
"""

url = re.compile('https?://(?:[-\w.]|(?:%[\da-fA-F]{2}))+')

netlocs_to_local = {
    'pic1.zhimg.com': "pic1_zhimg",
    'pic2.zhimg.com': 'pic2_zhimg',
    'pic3.zhimg.com': 'pic3_zhimg',
    'pic4.zhimg.com': 'pic4_zhimg',
}

to_download = []
cur_path = op.abspath(".")

def download_as(src, dest, check):
    to_download.append((src, dest))

def proceed_url(url):
    o = urlparse(url)
    subs = netlocs_to_local.get(o.netloc)
    if subs:
        localpath = op.join(cur_path, subs, o.path)
        download_as(url, localpath)
        return localpath
    else:
        return url

def proceed(node):
    if not isinstance(node, bs4, Tag):
        return
    attrs = node.attrs
    for key in list(attrs.keys()):
        value = attrs[key]
        urls = url.findall(value)
        flag = False
        for each in urls:
            t, new = proceed_url(each)
            if not t:
                continue
            flag = True
            value = value.replace(each, new)
        if flag:
            attrs[key] = value

    for each in node:
        proceed(each)

@wise
def read(filename: str):
    with open(filename) as f:
        s = f.read()
    proceed(bs4(s))
    print(str(s))

if __name__ == '__main__':
    import sys
    read(sys.argv[1:])