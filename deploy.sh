cat config.json
echo "okay? [yes/no]"

read answer
if test "$answer" != "yes" ; then
    echo "bye"
    exit 1
fi

sh build.sh && node deploy
