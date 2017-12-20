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

# Step 1. Convert metaprob parse tree to s-expression

def clojurefy(exp):
  if isinstance(exp, App):
    subs = tuple((clojurefy(e) for e in exp.subs))
    # metaprob call written f(...) => python tuple (f ...) => clojure list (f ...)
    return tuple(subs)
  elif isinstance(exp, Lit):
    return clojurefy_literal(exp.val)
  elif isinstance(exp, Var):
    return {'symbol': exp.name}
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
  return {'symbol': name}

def clojurefy_literal(val):
  # Convert metaprob literal (a venture value) to a python value
  if isinstance(val, vv.VentureInteger):
    return val.number
  elif isinstance(val, vv.VentureNumber):
    return val.number
  elif isinstance(val, vv.VentureString):
    return val.strng
  elif isinstance(val, vv.VentureBool):
    return val.boolean
  else:
    print '1 #%s %s#' % (repr(val), type(val))
    return val

def cons(thing, tup):
  return (thing,) + tuple(tup)

# Step 2. Write s-expression to file

# Python representation of clojure list (...): python tuple ?
# Python representation of clojure vector [...]: python list

def pprint(sexp, outfile):
  if isinstance(sexp, tuple):
    outfile.write('(')
    firstp = True
    for s in sexp:
      if firstp:
        firstp = False
      else:
        outfile.write(' ')
      pprint(s, outfile)
    outfile.write(')')
  elif isinstance(sexp, list):
    outfile.write('[')
    firstp = True
    for s in sexp:
      if firstp:
        firstp = False
      else:
        outfile.write(' ')
      pprint(s, outfile)
    outfile.write(']')
  elif isinstance(sexp, dict):
    if 'symbol' in sexp:
      outfile.write(sexp['symbol'])
    else:
      outfile.write(repr(sexp))
  elif isinstance(sexp, str):
    pprint_string(unicode(sexp), outfile)
  elif isinstance(sexp, unicode):
    pprint_string(sexp, outfile)
  elif isinstance(sexp, int):
    outfile.write(str(sexp))
  else:
    print '2 #%s %s#' % (repr(sexp), type(sexp))
    outfile.write(repr(sexp))

def pprint_string(u, outfile):
  outfile.write('"')
  if '"' in u:
    u = u.replace('"', '\\"')
  outfile.write(u)
  outfile.write('"')


#-----------------------------------------------------------------------------

def process_file(fname):
  with open(fname, 'r') as f:
    form = f.read()
  process_expression(form)

def process_expression(exp):
  exp = parse.parse_string(exp)
  outfile = sys.stdout
  pprint(clojurefy(exp), outfile)
  outfile.write('\n')

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('-e', '--eval', action='append',
    help="execute the given expression")
  parser.add_argument('-f', '--file', action='append',
    help="execute the given file")
  args = parser.parse_args()
  if args.file != None:
    for fname in args.file:
      process_file(fname)
  if args.eval != None:
    for exp in args.eval:
      process_expression(exp)
