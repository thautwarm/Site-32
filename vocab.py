from wisepy2 import wise
from nltk.tokenize import word_tokenize
from pathlib import Path
from nltk.corpus import stopwords
from collections import Counter
from string import punctuation
import sys
import re

def find_paths(suffix, p: Path):
    if not p.is_dir():
        if p.suffix == suffix:
            yield p
    else:
        for i in p.iterdir():
            if i == p:
                continue
            yield from find_paths(suffix, i)

backlist = [
    *stopwords.words("english"),
    "https", "http", "code-block", "--", "-",
]

@wise
def cmd(suffix:str, current_directory:str, *, most_common:int=10):
    _backlist = set(backlist)
    c = Counter()
    filt = re.compile("[^" + punctuation.replace("-", "") +"`'" + "]{2,}")
    def wording(words):
        for each in words:
            for word in filt.findall(each):
                word = word.lower()
                if word not in _backlist:
                    yield word

    for each in find_paths(suffix, Path(current_directory)):
        with each.open() as f:
            words = wording(word_tokenize(f.read()))
            c.update(words)
    for k, v in c.most_common(most_common):
        print(k, ':', v)


def main():
    cmd(sys.argv[1:])

main()