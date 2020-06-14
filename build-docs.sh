git checkout gh-pages
git merge -X theirs master
python manage.py --clean
python manage.py --build
python fix.py
git add -A
git commit -m "docs update"
git push origin gh-pages
git checkout master