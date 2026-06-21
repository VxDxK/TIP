@default:
    just -l

build:
    sbt clean
    sbt compile

test-variablesize:
    ./tip -variablesize wl examples/variablesize1.tip
    ./tip -variablesize wl examples/variablesize2.tip