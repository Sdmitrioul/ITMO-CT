#!/bin/bash

ja="../../java-advanced-2021/"
back="../homework/src"
src="../../java-advanced-2021/artifacts"

cd $ja &&
git pull &&
cd $back &&

echo &&
echo --------------------- &&
echo &&

cp -r $src/*.jar ./