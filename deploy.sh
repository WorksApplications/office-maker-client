set -eu

env=$1

if [ -z $env ]; then
    echo "no environment was given"
    exit 1
fi

cat config.${env}.json
echo "okay? [yes/no]"

read answer
if test "$answer" != "yes" ; then
    echo "bye"
    exit 1
fi

sh build.sh $env
node op/deploy $env
