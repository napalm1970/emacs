#!/bin/bash


if [ "$ACTION" == "remove" ];
then
   /usr/bin/synclient TouchpadOff=0
fi

if [ "$ACTION" == "add" ];
then
    /usr/bin/synclient TouchpadOff=1
fi



