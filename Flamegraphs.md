### Recording flamegraphs from jmh benchmarks

It's a little tricky since the benchmarks are forked into separate processes.
That said, you can run the target benchmark with `lein jmh '{:select :benchmark-name}'`

Then in another terminal window use: `ps | grep java` to find the forked jmh process
(There should be three processes, one for lein, one for the jmh runner, and one for the actual benchmark)

Bung that into `while true; do jstack $PID >> stack.txt; sleep 0.001; done`, and it'll emit a stack trace.

Finally you can use the tools from [Brendan Gregg](https://github.com/brendangregg/FlameGraph) to parse the stack into an SVG:
`./stackcollapse-jstack.pl < stack.txt | flamegraph.pl > stack.svg`

And you should be able to use any modern browser to open that: `google-chrome stack.svg`
