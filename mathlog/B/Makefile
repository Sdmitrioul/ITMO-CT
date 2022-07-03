all: compile run clean

compile:
	@javac *.java b/skroba/*.java b/skroba/*/*.java b/skroba/*/*/*.java

run:
	@java -Xmx1024M Main

clean:
	@rm -f *.class
	@rm -f b/skroba/*.class
	@rm -f b/skroba/*/*.class
	@rm -f b/skroba/*/*/*.class
