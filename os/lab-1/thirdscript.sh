#!/bin/bash

echo "Menu"
echo "1 - start nano"
echo "2 - start vim"
echo "3 - start links"
echo "4|\* - exit"

read -r com
case $com in
    1)
        vi
        ;;
    2)
        nano
        ;;
    3)
        links
        ;;
    4|*)
        echo "Exit 0"
        ;;
esac
