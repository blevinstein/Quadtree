empty :=
space := $(empty) $(empty)

JARS = $(subst $(space),:,$(wildcard lib/*.jar))
BUILDPATH = src:test:${JARS}
TESTPATH = src:test:build:${JARS}
RUNPATH = build:${JARS}

SCALAC_FLAGS = -feature

SCALA_SRCS = \
		$(wildcard src/*/*/*/*.scala) \
		$(wildcard src/*/*/*/*/*.scala) \
		$(wildcard test/*/*/*/*.scala) \
		$(wildcard test/*/*/*/*/*.scala)

TEST_SRCS = $(wildcard test/*/*/*/*Test.scala) \
						$(wildcard test/*/*/*/*/*Test.scala)
TESTS = $(subst /,.,$(subst test/,,$(subst .scala,,${TEST_SRCS})))

default: compile style tests

compile: ${SCALA_SRCS}
	scalac -cp ${BUILDPATH} ${SCALAC_FLAGS} ${SCALA_SRCS} -d build

grow: compile
	scala -cp ${RUNPATH} com.blevinstein.qt.grow.Driver

sim: compile
	scala -cp ${RUNPATH} com.blevinstein.qt.sim.Driver

shell:
	scala -cp ${RUNPATH}

style:
	${SCALASTYLE} -c scalastyle-config.xml src

tests: compile
	scala -cp ${TESTPATH} org.scalatest.run ${TESTS}

wc:
	wc -l ${SCALA_SRCS}

clean:
	rm -rf build/*
