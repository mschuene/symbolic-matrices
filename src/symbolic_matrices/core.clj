(ns symbolic-matrices.core
  (:use clojure.test)
  (:require [numeric.expresso.core :as symb]
            [numeric.expresso.construct :as con]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.generic-protocols :as gmp]
            [clojure.core.matrix.generic :refer :all]
            [clojure.core.matrix :as cm]))


(def symbolic (map->Specialisation
               {:add (fn [& args] (symb/simplify (con/cev '+ args)))
                :mul (fn [& args] (symb/simplify (con/cev '* args)))
                :sub (fn [& args] (symb/simplify (con/cev '- args)))
                :div (fn [& args] (symb/simplify (con/cev 'clojure.core// args)))
                :abs (fn [x] (if (number? x) (Math/abs (double x))
                                 (symb/ex' (abs x))))
                :scalar? symb/expression?
                :zero 0
                :one 1
                := (fn [a b] (if (and (number? a) (number? b))
                               (== a b) (= a b)))
                :supports-inequality? false
                :sqrt (fn [& args] (symb/simplify (con/cev 'sqrt args)))
                }))


(deftest test-generic-api
  (is (= [(symb/ex (+ a b))] (add symbolic '[a] '[b])))
  (is (= [(symb/ex (+ a 2.0))] (add symbolic '[a] [2.0])))
  (is (= [6.0] (add symbolic [4.0] [2.0])))
  (is (= [(symb/ex (+ a (- b)))] (sub symbolic '[a] '[b])))
  (is (= [(symb/ex (+ a -2.0))] (sub symbolic '[a] [2.0])))
  (is (= [2.0] (sub symbolic [4.0] [2.0])))
  (is (= [(symb/ex (* a b))] (mul symbolic '[a] '[b])))
  (is (= [(symb/ex (* a 2.0))] (mul symbolic '[a] [2.0])))
  (is (= [8.0] (mul symbolic [4.0] [2.0])))
  (is (= (symb/ex (sqrt (+ (** a 2) (** b 2) (** c 2)))) (length symbolic '[a b c]))))




(deftype Symbolic-wrapper [m])

(extend-protocol mp/PMatrixAdd
  Symbolic-wrapper
  (matrix-add [symb-wrapper a]
    (gmp/generic-matrix-add (.-m symb-wrapper) a symbolic)))

(defn make-symbolic [m] (Symbolic-wrapper. m))

(deftest test-use-normal-api-with-generic-wrapper
  "it is possible to use the normal core.matrix api with a wrapper type, that wraps a matrix implementation and forwards the protocols to the generic ones"
  (is (= [(symb/ex (+ a 1))
          (symb/ex (+ b 2))] (cm/add (make-symbolic '[a b]) [1 2]))))