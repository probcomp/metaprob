;; This file was automatically generated

(ns metaprob.examples.ad.code-rep
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  is_app
  app_subs
  is_var
  var_name
  is_lit
  lit_val
  is_lam
  lam_parts
  is_alt
  alt_parts
  alt_pred
  alt_cons
  alt_alt
  is_seq
  seq_subs
  is_tup
  tup_subs
  is_def
  def_pat
  def_expr
  is_ths
  is_wadr
  wadr_tag_expr
  wadr_expr
  is_spl
  spl_sub)

(define is_app (program [code] (eq (trace_get code) "application")))

(define app_subs (program [code] code))

(define is_var (program [code] (eq (trace_get code) "variable")))

(define
  var_name
  (program [code] (trace_get (lookup code (list "name")))))

(define is_lit (program [code] (eq (trace_get code) "literal")))

(define
  lit_val
  (program [code] (trace_get (lookup code (list "value")))))

(define is_lam (program [code] (eq (trace_get code) "program")))

(define
  lam_parts
  (program
    [code]
    (tuple (lookup code (list "pattern")) (lookup code (list "body")))))

(define is_alt (program [code] (eq (trace_get code) "if")))

(define
  alt_parts
  (program
    [code]
    (tuple
      (lookup code (list "predicate"))
      (lookup code (list "then"))
      (lookup code (list "else")))))

(define alt_pred (program [code] (lookup code (list "predicate"))))

(define alt_cons (program [code] (lookup code (list "then"))))

(define alt_alt (program [code] (lookup code (list "else"))))

(define is_seq (program [code] (eq (trace_get code) "block")))

(define seq_subs (program [code] code))

(define is_tup (program [code] (eq (trace_get code) "tuple")))

(define tup_subs (program [code] code))

(define is_def (program [code] (eq (trace_get code) "definition")))

(define def_pat (program [code] (lookup code (list "pattern"))))

(define
  def_expr
  (program
    [code]
    (lookup code (name_for_definiens (lookup code (list "pattern"))))))

(define is_ths (program [code] (eq (trace_get code) "this")))

(define is_wadr (program [code] (eq (trace_get code) "with_address")))

(define wadr_tag_expr (program [code] (lookup code (list "tag"))))

(define wadr_expr (program [code] (lookup code (list "expression"))))

(define is_spl (program [code] (eq (trace_get code) "splice")))

(define spl_sub (program [code] (lookup code (list "expression"))))

