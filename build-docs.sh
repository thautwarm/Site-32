git branch -D gh-pages
git checkout -b gh-pages
python manage.py --clean
python manage.py --build
git add -A
git commit -m "docs update"
git push origin gh-pages --force
git checkout master