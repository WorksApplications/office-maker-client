set -eu

env=$1

# prepare directories
mkdir -p dest
mkdir -p dest/public

# generate javascript
elm-make src/elm/Page/Map/Main.elm --output=dest/public/index.js --warn
elm-make src/elm/Page/Login/Main.elm --output=dest/public/login.js --warn
elm-make src/elm/Page/Master/Main.elm --output=dest/public/master.js --warn

# copy static files
cp -f src/style.css dest/public
cp -f doc/index.html doc/doc
mv doc/doc dest/public
cp -f doc/manual.pdf dest/public
cp -f images/default-user.png dest/public

# generate html
node op/generate-html $env
