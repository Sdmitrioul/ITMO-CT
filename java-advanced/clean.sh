#!/bin/bash

code="info/kgeorgiy/ja/skroba"

module="info.kgeorgiy.java.advanced.hello"    #"info.kgeorgiy.java.advanced.mapper"    #"info.kgeorgiy.java.advanced.concurrent"
var="server"  #client"  #"scalar" #"list"
cl="info.kgeorgiy.ja.skroba.hello.HelloUDPNonblockingServer"

rm $code/*/*.class && echo "Class files were deleted"