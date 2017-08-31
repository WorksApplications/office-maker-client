# prepare directories
mkdir -p dest
mkdir -p dest/public

# generate javascript
elm-make src/elm/Page/Map/Main.elm --output=dest/public/index.js --warn $1 &&
elm-make src/elm/Page/Login/Main.elm --output=dest/public/login.js --warn $1 &&
elm-make src/elm/Page/Master/Main.elm --output=dest/public/master.js --warn $1 &&

# copy static files
cp -f src/style.css dest/public
cp -f doc/index.html doc/doc
mv doc/doc dest/public

# generate html
node generate-html
