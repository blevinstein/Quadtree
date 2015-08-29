empty :=
space := $(empty) $(empty)

JARS = $(subst $(space),:,$(wildcard lib/*.jar))
BUILDPATH = src:test:${JARS}
RUNPATH = build:${JARS}

SCALAC_FLAGS =

DRIVER = com.blevinstein.qt.Driver

SCALA_SRCS = \
		$(wildcard src/*/*/*/*.scala) \
		$(wildcard src/*/*/*/*/*.scala) \
		$(wildcard test/*/*/*/*.scala) \
		$(wildcard test/*/*/*/*/*.scala)

TEST_SRCS = $(wildcard test/*/*/*/*Test.scala) \
						$(wildcard test/*/*/*/*/*Test.scala)
TESTS = $(subst /,.,$(subst test/,,$(subst .scala,,${TEST_SRCS})))

default: compile

compile: ${SCALA_SRCS}
	scalac -cp ${BUILDPATH} ${SCALAC_FLAGS} ${SCALA_SRCS} -d build

run: compile
	scala -cp ${RUNPATH} ${DRIVER}

shell:
	scala -cp ${RUNPATH}

tests: compile
	# TODO: add scala test support

clean:
	rm -rf build/*
