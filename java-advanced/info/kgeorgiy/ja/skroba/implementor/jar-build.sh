#!/bin/bash

backDir="../../../../../"
cd $backDir
dependency="../java-advanced-2021/artifacts/info.kgeorgiy.java.advanced.implementor.jar"
kr="info/kgeorgiy/java/advanced/implementor"

jar xf "$dependency" "$kr/Impler.class" "$kr/JarImpler.class" "$kr/ImplerException.class"
javac info/kgeorgiy/ja/skroba/implementor/*.java
echo "Manifest-Version: 1.0
Main-Class: info.kgeorgiy.ja.skroba.implementor.Implementor
Class-Path: $dependency" > Manifest

jar cmf Manifest info.kgeorgiy.ja.skroba.implementor.jar info/kgeorgiy/ja/skroba/implementor/*.class
rm Manifest
rm info/kgeorgiy/ja/skroba/implementor/*.class