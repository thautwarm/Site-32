import json
import os.path as path
import os
from dateutil.parser import parse as parse_date
from datetime import datetime
from wisepy.talking import Talking
from io import StringIO
cmd = Talking()


@cmd
def build():
    dir, _ = path.split(__file__)
    dir = path.join(dir, 'src')

    with open(path.join(dir, 'dynamic.json'), 'rb') as f:
        dy : list = json.load(f)

    for each in dy:
        each['release_data'] = parse_date(each['release_data'])
    dy = sorted(dy, key=lambda each: each['release_data'], reverse=True)
    with open(path.join(dir, 'index.rst'), 'w') as w, \
         open(path.join(dir, 'index.rst.template', 'r')) as r:
        write = w.write
        write(r.read())
        write('\n')

        for each in dy:
            title = each['title']
            keywords = each['keywords']
            time = str(each['release_date'])
            write(title)
            write('\n')
            write('-' * len(title))
            write('- keywords: {}'.format(' , '.join(f":code:`{each}`" for each in keywords)))
            write(f'- datetime: {time}')
            write('\n')

    os.system('sphinx-build -b html ./src ./')