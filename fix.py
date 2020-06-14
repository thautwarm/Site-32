import sys
import os
ver = sys.argv[1]

with open(f'/_static/bootstrap-sphinx.css') as f:
    src = f.read()

with open(f'/_static/bootstrap-sphinx.css', 'w') as f:
    f.write(src.replace("Readable", 'readable'))