#!/bin/bash

standardApiLink="https://docs.oracle.com/en/java/javase/11/docs/api"
kgeorgiyModulesClassesPath="../../../../../../../java-advanced-2021/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor"

javadoc Implementor.java -d _javadoc -link $standardApiLink -private -author -version $kgeorgiyModulesClassesPath/JarImpler.java $kgeorgiyModulesClassesPath/Impler.java $kgeorgiyModulesClassesPath/ImplerException.java