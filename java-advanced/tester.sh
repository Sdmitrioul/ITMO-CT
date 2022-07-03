#!/bin/bash

code="info/kgeorgiy/ja/skroba"

module="info.kgeorgiy.java.advanced.hello"    #"info.kgeorgiy.java.advanced.mapper"    #"info.kgeorgiy.java.advanced.concurrent"
var="client"  #client"  #"scalar" #"list"
cl="info.kgeorgiy.ja.skroba.hello.HelloUDPClient"

#/bin/bash update.sh &&

echo &&
echo --------------------- &&
echo &&

#jar xf "jsoup-1.8.1.jar"
#javac -classpath "info.kgeorgiy.java.advanced.crawler.jar" $code/crawler/*.java && echo "Good compiling" &&
javac $code/*/*.java && echo "Good compiling" &&

echo &&
echo --------------------- &&
echo &&

java -cp . -p . -m $module $var $cl

echo &&
echo --------------------- &&
echo &&

/bin/bash clean.sh &&

echo &&
echo --------------------- &&
echo &&

echo +++++ succsessful +++++

