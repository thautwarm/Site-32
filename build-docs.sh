cd docs
CUR="`git branch | grep \"*\" | cut -c 2-`"
echo $CUR
cd ../
git checkout gh-pages
git merge -X theirsmaster
python manage.py --clean
python manage.py --build
git add -A
git commit -m "docs update"
git push origin gh-pages
git checkout $CUR