import json
import os.path as path
import os
from dateutil.parser import parse as parse_date
from datetime import datetime
from wisepy2 import wise
from Redy.Tools.PathLib import Path
from io import StringIO
from textwrap import indent

def raw_html(html):
    html = indent(html, prefix = '    ')
    return f"""
.. raw:: html


{html}
"""

panel_styles = ['success', 'primary', 'warning']
n_panel_styles = len(panel_styles)
panel_count = 0
def card(title: str, link: str, keywords: list, time: datetime):
    global panel_count
    time = time.strftime("%a, %B %d, %Y.&nbsp; %H: %M ")
    keywords = '&nbsp;,&nbsp;'.join(keywords)
    panel_count += 1
    return raw_html(f"""
<br>
<div class="panel panel-{panel_styles[panel_count % n_panel_styles]}">
  <div class="panel-heading">{title}</div>
  <div class="panel-body">
    keywords: {keywords}
  </div>

  <div class="panel-footer">
    <a href="{link}">Check</a>
    <span class="pull-right">{time}</span>
  </div>
</div>
""")


def build():
    dir, _ = path.split(__file__)
    dir = path.join(dir, 'src')

    with open(path.join(dir, 'dynamic.json'), 'rb') as f:
        dy : list = json.load(f)

    for each in dy:
        each['release_date'] = parse_date(each['release_date'])
    dy = sorted(dy, key=lambda each: each['release_date'], reverse=True)
    with open(path.join(dir, 'index.rst'), 'w', encoding='utf8') as w, \
         open(path.join(dir, 'index.rst.template'), 'r', encoding='utf8') as r:
        write = w.write
        write(r.read())
        write('\n')
        for each in dy:
            title = each['title']
            where = each['where']
            where = "./" + "/".join(where.split('.')) + '.html'
            time = each['release_date']
            keywords = each['keywords']
            write('\n')
            write(card(
                title=title,
                link = where,
                time = time,
                keywords=keywords))

    os.system('sphinx-build -b html ./src ./')
    for each_static in Path("./src/BackupStatic").list_dir():
      each_static.move_to("./Backup/")


preserved = [
  'src',
  '.gitignore',
  'manage.py',
  'vocab.py',
  '.vscode',
  'localize.py',
  'favicon.ico',
  '.nojekyll',
  '.',
  '.git',
  "build-docs.sh"
]

def clean():
    for each in Path('.').list_dir():
        filename = each.relative()
        if filename in preserved:
          continue
        each.delete()

def dispatch(build: bool=False, clean: bool=False):
  g = globals()
  if clean:
    g['clean']()
  if build:
    g['build']()
  
if __name__ == '__main__':
    wise(dispatch)()
