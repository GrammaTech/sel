import sel
import sys

#
# some examples of scripting using the sel rest api
#

sel_path = '/home/rcorman/synth/python/'
if sel_path not in sys.path:
    sys.path.append(sel_path)

import sel

initializers = {
  "build-command": "make foo",
  "artifacts": [ "foo" ],
  "clang-class": "BUG-INJECTOR/UTILITY::BI-CLANG-TRACEABLE-STYLEABLE",
  "compilation-database": [{
      "file": "/home/rcorman/quicklisp/local-projects/sel/test/etc/rest-multi-file/foo.cpp",
      "directory": "/home/rcorman/quicklisp/local-projects/sel/test/etc/rest-multi-file",
      "command": "make"
      },
      {
      "file": "/home/rcorman/quicklisp/local-projects/sel/test/etc/rest-multi-file/bar.cpp",
      "directory": "/home/rcorman/quicklisp/local-projects/sel/test/etc/rest-multi-file",
      "command": "make"
      }],
  "path": "/home/rcorman/quicklisp/local-projects/sel/test/etc/rest-multi-file"
  }

sel.create_client()
soft = sel.create_software('BUG-INJECTOR/UTILITY::BI-CLANG-TRACEABLE-STYLEABLE-PROJECT', initializers)
pop = sel.create_population(None, { 'type': 'clang-project', 'sids': [soft] })
mutation = sel.create_mutation('sel::simple-cut', 160, [1])
async = sel.create_async_job('my-job', pop, "cl::identity", 1)
tests = sel.create_tests(
    { "tests":
      [{"program-name":"~/quicklisp/local-projects/sel/test/etc/multi-file/test.sh",
        "program-args":[":BIN","1"]},
       {"program-name":"~/quicklisp/local-projects/sel/test/etc/multi-file/test.sh",
        "program-args":[":BIN","2"]}]})
instrumented = sel.create_instrumented(soft)
traced = sel.create_trace(instrumented, tests, "~/synth/bin/foo.out")
scion = sel.create_scion('BUG-INJECTOR/SCIONS/CLANG::DIVISION-BY-ZERO-INT')
scion2 = sel.create_scion('BUG-INJECTOR/SCIONS/CLANG::ARITHMETIC-OVERFLOW-MULTIPLICATION-INT')
scion3 = sel.create_scion('BUG-INJECTOR/SCIONS/CLANG::DIVISION-BY-ZERO-INT')

trace_results = sel.create_trace_results('my-trace', traced, scion2)
sel.get_trace_results(trace_results)

pp = pprint.PrettyPrinter(indent=2)
pp.pprint(sel.get_trace_results(trace_results))

soft = sel.create_software('BUG-INJECTOR/UTILITY::BI-CLANG-TRACEABLE-STYLEABLE-PROJECT', initializers)
tests = sel.create_tests(
    { "tests":
      [{"program-name":"~/quicklisp/local-projects/sel/test/etc/multi-file/test.sh",
        "program-args":[":BIN","1"]},
       {"program-name":"~/quicklisp/local-projects/sel/test/etc/multi-file/test.sh",
        "program-args":[":BIN","2"]}]})
instrumented = sel.create_instrumented(soft)
traced = sel.create_trace(instrumented, tests, "~/synth/bin/foo.out")
scion = sel.create_scion('BUG-INJECTOR/SCIONS/CLANG::ARITHMETIC-OVERFLOW-MULTIPLICATION-INT')
trace_results = sel.create_trace_results('my-trace', traced, scion)
sel.get_trace_results(trace_results)
#########
soft = sel.create_software('BUG-INJECTOR/UTILITY::BI-CLANG-TRACEABLE-STYLEABLE', 
    { 'path': '/home/rcorman/quicklisp/local-projects/bug-injector/test/etc/overflow-underflow-multiplication-regression-tests/overflow2.c' })

tests = sel.create_tests(
    { "tests":
      [{"program-name":"~/quicklisp/local-projects/sel/test/etc/multi-file/test.sh",
        "program-args":[":BIN","1"]}]})

instrumented = sel.create_instrumented(soft)
traced = sel.create_trace(instrumented, tests, "~/synth/bin/foo.out")
scion = sel.create_scion('BUG-INJECTOR/SCIONS/CLANG::ARITHMETIC-OVERFLOW-MULTIPLICATION-INT')
trace_results = sel.create_trace_results('my-trace', traced, scion)
sel.get_trace_results(trace_results)

./bi --verbose 2 --out-dir '/home/rcorman/synth/' '/home/rcorman/quicklisp/local-projects/bug-injector/test/etc/overflow-underflow-multiplication-regression-tests/overflow2.c' 'arithmetic-overflow-multiplication-int' test.sh 1

(bi/bi::bi  "/home/rcorman/quicklisp/local-projects/bug-injector/test/etc/overflow-underflow-multiplication-regression-tests/overflow2.c"
  "arithmetic-overflow-multiplication-int"
  "test.sh"
  1
  :verbose 2
  :out-dir "/home/rcorman/synth/"
)

(bi/scion::resolve-scions-from-scions "arithmetic-overflow-multiplication-int")
(bi/scion::iota (n-traces (traces *instrumented*))) ; should return trace data

(trace-db::get-trace *trace-db* 0)
