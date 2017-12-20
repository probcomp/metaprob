# (setq python-indent-offset 2)

# Don't forget:
#   . ../clean/ve/bin/activate
#   python setup.py  build
#   ./pythenv.sh metaprob -e 'print(2+2)'
#   cd ../dontknow
#   ../metaprob/pythenv.sh python foo.py

# N.b. virtualenv and #! do not interact nicely.  (#! overrides the
# virtualenv for location of python executable.)


import sys, argparse

from metaprob.parser import parse
import metaprob.sp 

import metaprob.types
from metaprob.trace import ITrace
from metaprob.trace import NullTrace
from metaprob.trace import metaprob_collection_to_python_list
from metaprob.trace import metaprob_nil
from metaprob.trace import python_list_to_metaprob_array
from metaprob.trace import python_list_to_metaprob_list
from metaprob.types import Alt
from metaprob.types import App
from metaprob.types import Def
from metaprob.types import Exp # pylint: disable=unused-import
from metaprob.types import Lam
from metaprob.types import Lit
from metaprob.types import Seq
from metaprob.types import Spl
from metaprob.types import Ths
from metaprob.types import Trace # pylint: disable=unused-import
from metaprob.types import Tup
from metaprob.types import Unq
from metaprob.types import Var
from metaprob.types import WAdr

import venture.lite.value as vv

symbol_marker = '__symbol__'

# Step 1. Convert metaprob parse tree to s-expression

def clojurefy(exp):
  if isinstance(exp, App):
    subs = tuple((clojurefy(e) for e in exp.subs))
    # metaprob call written f(...) => python tuple (f ...) => clojure list (f ...)
    return tuple(subs)
  elif isinstance(exp, Lit):
    # exp.val is a VentureValue
    return venture_to_python(exp.val)
  elif isinstance(exp, Var):
    return sym(exp.name)
  elif isinstance(exp, Lam):
    return (sym('fn'), clojurefy(exp.pat), clojurefy(exp.body))
  elif isinstance(exp, Alt):
    return (sym('if'), clojurefy(exp.pred), clojurefy(exp.cons), clojurefy(exp.alt))
  elif isinstance(exp, Seq):
    subs = tuple((clojurefy(e) for e in exp.subs))
    if len(subs) == 1:
      return subs[0]
    else:
      return cons(sym('do'), subs)
  elif isinstance(exp, Ths):
    return (sym('this'))
  elif isinstance(exp, WAdr):
    return (sym('with-address'), clojurefy(exp.tag), clojurefy(exp.expr))
  elif isinstance(exp, Tup):
    # metaprob tuple written [...] => python list [...] => clojure vector [...]
    return list((clojurefy(e) for e in exp.subs))
  elif isinstance(exp, Def):
    return (sym('def'), clojurefy(exp.pat), clojurefy(exp.expr))
  elif isinstance(exp, Unq):
    print sys.stderr, "Found unquote outside quoted context"
    return '*error*'
  assert False, "Unknown expression type %s" % (repr(exp),)

def sym(name):
  return {symbol_marker: name}

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

def cons(thing, tup):
  return (thing,) + tuple(tup)

# Step 2. Write s-expression to file

# Python representation of clojure list (...): python tuple ?
# Python representation of clojure vector [...]: python list

def print_as_clojure(sexp, outfile):
  if isinstance(sexp, bool):
    if sexp:
      outfile.write('true')
    else:
      outfile.write('false')
  elif isinstance(sexp, tuple):
    outfile.write('(')
    firstp = True
    for s in sexp:
      if firstp:
        firstp = False
      else:
        outfile.write(' ')
      print_as_clojure(s, outfile)
    outfile.write(')')
  elif isinstance(sexp, list):
    outfile.write('[')
    firstp = True
    for s in sexp:
      if firstp:
        firstp = False
      else:
        outfile.write(' ')
      print_as_clojure(s, outfile)
    outfile.write(']')
  elif isinstance(sexp, dict):
    #print '[dict %s]' % sexp
    if symbol_marker in sexp:
      outfile.write(sexp[symbol_marker])
    else:
      outfile.write('{')
      firstp = True
      if value_marker in sexp:
        outfile.write(':v')
        outfile.write(' ' )
        print_as_clojure(sexp[value_marker], outfile)
        firstp = False
      for (key, val) in sexp.items():
        if key != value_marker:
          if firstp:
            firstp = False
          else:
            outfile.write(' ' )
          print_as_clojure(key, outfile)
          outfile.write(' ' )
          print_as_clojure(val, outfile)
      outfile.write('}')
  elif isinstance(sexp, str):
    print_as_clojure_string(unicode(sexp), outfile)
  elif isinstance(sexp, unicode):
    print_as_clojure_string(sexp, outfile)
  elif isinstance(sexp, int):
    outfile.write(str(sexp))
  elif isinstance(sexp, float):
    outfile.write(str(sexp))
  else:
    print '[#2 %s %s#]' % (repr(sexp), type(sexp))
    outfile.write(repr(sexp))

def print_as_clojure_string(u, outfile):
  outfile.write('"')
  if '"' in u:
    u = u.replace('"', '\\"')
  outfile.write(u)
  outfile.write('"')

value_marker = '__v__'

def convert_trace(exp):
  return convert_trace_1(metaprob.types.reify_exp_to_venture(exp))

def convert_trace_1(tr):
  nontrivial = False
  d = {}
  if tr.has():
    # .get() returns a VentureValue, according to comment in trace.py.
    #print 'vv %s %s' % (tr.get(), type(tr.get()))
    d[value_marker] = venture_to_python(tr.get())
  else:
    nontrival = True
  for key in tr.subkeys():    # List[vv.VentureValue]
    #print 'vk %s %s' % (key, type(key))
    with tr.subtrace(key) as sub:
      k = venture_to_python(key)
      d[k] = convert_trace_1(sub)
      nontrivial = True
  if nontrivial:
    return d
  else:
    return d[value_marker]

#-----------------------------------------------------------------------------

def process_file(fname, converter):
  with open(fname, 'r') as f:
    form = f.read()
  process_expression(form, converter)

def process_expression(exp, converter):
  exp = parse.parse_string(exp)
  outfile = sys.stdout
  print_as_clojure(converter(exp), outfile)
  outfile.write('\n')

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('-t', '--trace', action='store_true',
                      help="print trace version of code")
  parser.add_argument('-c', '--clojure', action='store_true',
                      help="print clojure version of code (default)")
  parser.add_argument('-e', '--eval', action='append',
    help="execute the given expression")
  parser.add_argument('-f', '--file', action='append',
    help="execute the given file")
  args = parser.parse_args()
  if args.trace:
    converter = convert_trace
  else:
    converter = clojurefy
  if args.file != None:
    for fname in args.file:
      process_file(fname, converter)
  if args.eval != None:
    for exp in args.eval:
      process_expression(exp, converter)
