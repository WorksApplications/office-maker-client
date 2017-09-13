cat config.json
echo "okay? [yes/no]"

read answer
case $answer in
    yes)
        sh build.sh && node deploy
        ;;
    *)
        echo "bye"
        ;;
esac
