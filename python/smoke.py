# Some setup gleaned as needed from test_smoke.py

import os.path as p

self_path = p.dirname(p.abspath(__file__))
root_path = p.dirname(self_path)
prelude = p.join(root_path, "src", "prelude.vnts")
mem = p.join(root_path, "src", "mem.vnts")
crp = p.join(root_path, "src", "crp.vnts")
normal_normal = p.join(root_path, "examples", "normal-normal.vnts")
files = [prelude, mem]

counter = 1

def run(form, files=[], seed=1):
  global counter
  print 'smoke%s = () -> { %s; };\n' % (counter, form)
  counter += 1

# The following were extracted from test_smoke.py using an emacs keyboard macro

run('((x) -> x)(2)')
run('normal(2, 1)', seed=1)
run('{{ }}')
run('{{ }} has_value')
run('{ t = {{ }}; t := 5; *t; }')
run('{ t = {{ 6 }}; *t; }')
run('is_pair(pair(1, {{2}}))', files=[prelude])
run('first(pair(1, {{2}}))', files=[prelude])
run('*rest(pair(1, {{2}}))', files=[prelude])
run('''{
  t = {{ }};
  (x, score) = py_propose(normal, [0, 1], {{ }}, {{ }}, t);
  (score, x, *t); }''', seed=1)
run('''{
  t2 = {{ }};
  (x, score) = py_propose(normal, [0, 1], {{ }}, {{1}}, t2);
  (score, x, *t2); }''')
run('''{
  t2 = {{ }};
  (x, score) = py_propose(normal, [0, 1], {{1}}, {{ }}, t2);
  (score, x, *t2); }''')
run('normal_normal(0, 1, 1)', files=[prelude, normal_normal], seed=1)
run('''{
  model = () ~> normal_normal(0, 100, 1);
  t2 = {{ }};
  py_propose(model, [], {{ }}, {{ }}, t2);
  subt = t2[/$(prob_prog_name(normal_normal))/];
  (*subt[/x/], *subt); }''', files=[prelude, normal_normal], seed=1)
run('''{
  model = () ~> normal_normal(0, 1, 1);
  t1 = {{ */$(prob_prog_name(normal_normal))/ = 5 }};
  t2 = {{ }};
  score = propose1(model, [], {{ }}, t1, t2);
  subt = t2[/$(prob_prog_name(normal_normal))/];
  (score, *subt[/x/], *subt); }''', files=[prelude, normal_normal], seed=1)
run('''{
  t2 = {{ }};
  t4 = {{ }};
  py_propose(proposer_of(normal_normal), [[0, 1, 1], {{ }}, {{ }}, t2], {{ }}, {{ }}, t4);
  (*t2[/x/], t4);}''', files=[prelude, normal_normal], seed=1)
run('''{
  t2 = {{ }};
  t4 = {{ }};
  ((y, in_score), out_score) = py_propose(proposer_of(normal_normal), [[0, 1, 1], {{ }}, {{5}}, t2], {{ }}, {{ }}, t4);
  (out_score, in_score, y, *t2[/x/], t4);
  }''', files=[prelude, normal_normal], seed=1)
run('''{
  t2 = {{ }};
  t4 = {{ */3/then/5/post_sample/normal/ = 7 }};
  ((y, in_score), out_score) = py_propose(proposer_of(normal_normal), [[0, 1, 1], {{ }}, {{5}}, t2], t4, {{ }}, {{ }});
  (out_score, in_score, y, *t2[/x/], t4);
  }''', files=[prelude, normal_normal], seed=1)
run('''{
  site = /1/app/;
  t = {{*$site = 4}};
  *t[site];}''', files=[prelude])
run('''{
  doit = () -> {
    {{ */fred/brooks/ = 3 }};
  };
  (tr, score) = py_propose(doit, [], {{ }}, {{ }}, {{ }});
  (*tr[/fred/brooks/], score);}''', files=[prelude])
run('{{ {{ }} }}')
run('''
double_gaussian = () ~> {
  f = mem((x) ~> normal(0, 1));
  (f(1), f(1));
};

py_propose(double_gaussian, [], {{ }}, {{ }}, {{ }})
  ''',
    files=files, seed=1)
run('''
double_gaussian = () ~> {
  f = mem((x) ~> normal(0, 1));
  (f(1), f(2));
};

py_propose(double_gaussian, [], {{ }}, {{ }}, {{ }})
''',
    files=files, seed=1)
run('''
double_gaussian = () ~> {
  f = mem((x) ~> normal(0, 1));
  (f(3), f(3));
};

t1 = {{ */0/f/$(prob_prog_name(mem))/cache/3/$(double_gaussian[/source/body/0/f/1/])/normal/ = 2 }};
py_propose(double_gaussian, [], {{ }}, t1, {{ }})
  ''', files=files, seed=1)
run('propose1(factor, [-3], {{ }}, {{ }}, {{ }})', files=[prelude])
run('py_propose(() -> { &this; }, [], {{ }}, {{ }}, {{ }})')
run('''{
  t = {{ }};
  (x, score) = py_propose(() ~> { with_address /$&this/fred/: normal(0, 1); }, [], {{ }}, {{ }}, t);
  (score, x, *t[/fred/normal/]); }''', files=[prelude], seed=1)
run('''{
  t = {{ }};
  fred_norm = (root) ~> with_address /$root/fred/: normal(0, 1);
  tagged_norm = () ~> { fred_norm(&this); };
  (x, score) = py_propose(tagged_norm, [], {{ }}, {{ }}, t);
  (score, x, *t[/fred/normal/]); }''', files=[prelude], seed=1)
run('''{
  my_norm = (i) ~> normal(0, 1);
  doit = () ~> map(my_norm, range(2));
  t = {{ }};
  (lst, score) = py_propose(doit, [], {{ }}, {{ }}, t);
  site1 = /$(prob_prog_name(map))/0/$(prob_prog_name(my_norm))/normal/;
  site2 = /$(prob_prog_name(map))/1/$(prob_prog_name(my_norm))/normal/;
  (score, lst, *t[site1], *t[site2]); }''', files=[prelude], seed=1)
run('''{
  t = {{ }};
  my_norm = (i) ~> normal(0, 1);
  doit = () ~> map(my_norm, range(2));
  site1 = /$(prob_prog_name(map))/0/$(prob_prog_name(my_norm))/normal/;
  site2 = /$(prob_prog_name(map))/1/$(prob_prog_name(my_norm))/normal/;
  cons = {{*$site1 = 1, *$site2 = 2}};
  (lst, score) = py_propose(doit, [], {{ }}, cons, t);
  (score, lst, *t[site1], *t[site2]); }''', files=[prelude], seed=1)
run('''{
  doit = () ~> {
    root = &this;
    my_norm = (i) ~> with_address /$root/$i/: normal(0, 1);
    map(my_norm, range(2));
  };
  t = {{ }};
  (lst, score) = py_propose(doit, [], {{ }}, {{ }}, t);
  site1 = /0/normal/;
  site2 = /1/normal/;
  (score, lst, *t[site1], *t[site2]); }''', files=[prelude], seed=1)
