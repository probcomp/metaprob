# (setq python-indent-offset 2)

# Convert native metaprob infix syntax to an s-expression encoding of a trace

# Don't forget:
#   . ../clean/ve/bin/activate

#   python setup.py  build
#   ./pythenv.sh metaprob -e 'print(2+2)'
#   cd ../metaprob
#   ../metaprob/pythenv.sh python foo.py

# N.b. virtualenv and #! do not interact nicely.  (#! overrides the
# virtualenv for location of python executable.)


import sys, argparse

from metaprob.parser.parse import parse_string
from metaprob.types import reify_exp_to_venture

import venture.lite.value as vv

abbreviate_trivial = False

# Argument is a metaprob parse tree represented as a trace, the result
# of parse.parse_string.
# This writes the trace to outfile in s-expression format.

def emit_trace(tr, outfile):
  keylist = tr.subkeys()
  if abbreviate_trivial and len(keylist) == 0 and tr.has():
    emit_value(venture_to_python(tr.get()), outfile)
  else:
    outfile.write('(')
    if tr.has():
      emit_value(venture_to_python(tr.get()), outfile)
    else:
      outfile.write(':none')
    for key in keylist:    # List[vv.VentureValue]
      with tr.subtrace(key) as sub:
        outfile.write(' ')
        k = venture_to_python(key)
        emit_value(k, outfile)
        outfile.write(' ')
        emit_trace(sub, outfile)
    outfile.write(')')

def emit_value(val, outfile):
  if isinstance(val, bool):
    if val:
      outfile.write('true')
    else:
      outfile.write('false')
  elif isinstance(val, str):
    print_as_clojure_string(unicode(val), outfile)
  elif isinstance(val, unicode):
    print_as_clojure_string(val, outfile)
  elif isinstance(val, int):
    outfile.write(str(val))
  elif isinstance(val, float):
    outfile.write(str(val))
  else:
    print '[odd value %s %s#]' % (repr(val), type(val))
    outfile.write(repr(val))

def print_as_clojure_string(u, outfile):
  outfile.write('"')
  if '"' in u:
    u = u.replace('"', '\\"')
  outfile.write(u)
  outfile.write('"')

# Convert metaprob literal to s-expression

def venture_to_python(vval):
  # Convert metaprob literal (a venture value) to a python value
  if isinstance(vval, vv.VentureInteger):
    return vval.number
  elif isinstance(vval, vv.VentureNumber):
    return vval.number
  elif isinstance(vval, vv.VentureString):
    return vval.strng
  elif isinstance(vval, vv.VentureBool):
    return vval.getBool()
  else:
    print '[#1 %s %s#]' % (repr(vval), type(vval))
    return vval

# Convert trace to clojure syntax, allowing the trace to be
# reconstructed when read into clojure.

def emit_exp_as_trace(tr, outfile):
  emit_trace(reify_exp_to_venture(tr), outfile)
  outfile.write('\n')

def process_file(fname, emitter):
  with open(fname, 'r') as f:
    strng = f.read()
    process_expression(strng, emitter)

def process_expression(strng, emitter):
  emitter(parse_string(strng))

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('-e', '--eval', action='append',
                      help="transduce the given expression")
  parser.add_argument('-f', '--file', action='append',
                      help="transduce the given file")
  args = parser.parse_args()
  outfile = sys.stdout
  emitter = lambda tr: \
              emit_exp_as_trace(tr, outfile); outfile.write('\n')
  if args.file != None:
    for fname in args.file:
      process_file(fname, emitter)
  if args.eval != None:
    for exp in args.eval:
      process_expression(exp, emitter)
  outfile.flush()
